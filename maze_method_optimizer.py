#!/usr/bin/env python3
"""
Busca o melhor metodo de geracao de labirinto (51x51 = 25x25 celulas)
para maximizar dificuldade percebida por humanos.

Estrategia:
1) Testa varios metodos de geracao.
2) Faz uma triagem rapida por metodo.
3) Reavalia os melhores candidatos com 10 rodadas por sentido (A->B e B->A).
4) Seleciona o melhor metodo/candidato e salva imagens em outputs/.
"""

from __future__ import annotations

import argparse
import random
import struct
import time
import zlib
from collections import deque
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence, Tuple

N, E, S, W = 1, 2, 4, 8
DIRECTIONS: Sequence[Tuple[int, int, int, int]] = (
    (-1, 0, N, S),
    (0, 1, E, W),
    (1, 0, S, N),
    (0, -1, W, E),
)

ROWS = 25
COLS = 25
FINAL_EVAL_XOR = 0xC2B2AE35


@dataclass
class Evaluation:
    avg_steps: float
    difficulty_ratio: float
    shortest: int
    results_a_to_b: List[int]
    results_b_to_a: List[int]
    node_visits_avg: Optional[List[int]] = None
    edge_visits_avg: Optional[Dict[Tuple[int, int], int]] = None


@dataclass
class Candidate:
    method: str
    seed: int
    maze: List[List[int]]
    screening: Evaluation


def rc_to_idx(r: int, c: int, cols: int) -> int:
    return r * cols + c


def idx_to_rc(idx: int, cols: int) -> Tuple[int, int]:
    return idx // cols, idx % cols


def carve_between(maze: List[List[int]], r: int, c: int, nr: int, nc: int) -> None:
    dr, dc = nr - r, nc - c
    if dr == -1 and dc == 0:
        maze[r][c] |= N
        maze[nr][nc] |= S
    elif dr == 1 and dc == 0:
        maze[r][c] |= S
        maze[nr][nc] |= N
    elif dr == 0 and dc == 1:
        maze[r][c] |= E
        maze[nr][nc] |= W
    elif dr == 0 and dc == -1:
        maze[r][c] |= W
        maze[nr][nc] |= E
    else:
        raise ValueError("Celulas nao adjacentes para abertura de passagem.")


def build_grid_neighbors(rows: int, cols: int) -> List[List[int]]:
    neighbors = [[] for _ in range(rows * cols)]
    for r in range(rows):
        for c in range(cols):
            idx = rc_to_idx(r, c, cols)
            if r > 0:
                neighbors[idx].append(rc_to_idx(r - 1, c, cols))
            if c < cols - 1:
                neighbors[idx].append(rc_to_idx(r, c + 1, cols))
            if r < rows - 1:
                neighbors[idx].append(rc_to_idx(r + 1, c, cols))
            if c > 0:
                neighbors[idx].append(rc_to_idx(r, c - 1, cols))
    return neighbors


def generate_dfs(rows: int, cols: int, rng: random.Random) -> List[List[int]]:
    maze = [[0 for _ in range(cols)] for _ in range(rows)]
    visited = [[False for _ in range(cols)] for _ in range(rows)]

    stack: List[Tuple[int, int]] = [(0, 0)]
    visited[0][0] = True

    while stack:
        r, c = stack[-1]
        candidates = []
        for dr, dc, _, _ in DIRECTIONS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and not visited[nr][nc]:
                candidates.append((nr, nc))

        if not candidates:
            stack.pop()
            continue

        nr, nc = rng.choice(candidates)
        carve_between(maze, r, c, nr, nc)
        visited[nr][nc] = True
        stack.append((nr, nc))

    return maze


def generate_prim(rows: int, cols: int, rng: random.Random) -> List[List[int]]:
    maze = [[0 for _ in range(cols)] for _ in range(rows)]
    visited = [[False for _ in range(cols)] for _ in range(rows)]

    start_r, start_c = rng.randrange(rows), rng.randrange(cols)
    visited[start_r][start_c] = True

    frontier: List[Tuple[int, int, int, int]] = []

    def add_frontier(r: int, c: int) -> None:
        for dr, dc, _, _ in DIRECTIONS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and not visited[nr][nc]:
                frontier.append((r, c, nr, nc))

    add_frontier(start_r, start_c)

    while frontier:
        idx = rng.randrange(len(frontier))
        r, c, nr, nc = frontier[idx]
        frontier[idx] = frontier[-1]
        frontier.pop()

        if visited[nr][nc]:
            continue

        carve_between(maze, r, c, nr, nc)
        visited[nr][nc] = True
        add_frontier(nr, nc)

    return maze


class UnionFind:
    def __init__(self, n: int):
        self.parent = list(range(n))
        self.rank = [0 for _ in range(n)]

    def find(self, x: int) -> int:
        while self.parent[x] != x:
            self.parent[x] = self.parent[self.parent[x]]
            x = self.parent[x]
        return x

    def union(self, a: int, b: int) -> bool:
        ra = self.find(a)
        rb = self.find(b)
        if ra == rb:
            return False

        if self.rank[ra] < self.rank[rb]:
            ra, rb = rb, ra

        self.parent[rb] = ra
        if self.rank[ra] == self.rank[rb]:
            self.rank[ra] += 1
        return True


def generate_kruskal(rows: int, cols: int, rng: random.Random) -> List[List[int]]:
    maze = [[0 for _ in range(cols)] for _ in range(rows)]
    uf = UnionFind(rows * cols)

    edges: List[Tuple[int, int, int, int]] = []
    for r in range(rows):
        for c in range(cols):
            if r + 1 < rows:
                edges.append((r, c, r + 1, c))
            if c + 1 < cols:
                edges.append((r, c, r, c + 1))

    rng.shuffle(edges)

    opened = 0
    for r, c, nr, nc in edges:
        a = rc_to_idx(r, c, cols)
        b = rc_to_idx(nr, nc, cols)
        if uf.union(a, b):
            carve_between(maze, r, c, nr, nc)
            opened += 1
            if opened == rows * cols - 1:
                break

    return maze


def generate_wilson(rows: int, cols: int, rng: random.Random) -> List[List[int]]:
    maze = [[0 for _ in range(cols)] for _ in range(rows)]
    total = rows * cols
    in_tree = [False for _ in range(total)]
    grid_neighbors = build_grid_neighbors(rows, cols)

    root = rng.randrange(total)
    in_tree[root] = True
    remaining = total - 1

    while remaining > 0:
        start = rng.randrange(total)
        while in_tree[start]:
            start = rng.randrange(total)

        path = [start]
        path_pos = {start: 0}
        current = start

        while True:
            nxt = rng.choice(grid_neighbors[current])

            if nxt in path_pos:
                cut = path_pos[nxt]
                for rem in path[cut + 1 :]:
                    del path_pos[rem]
                path = path[: cut + 1]
            else:
                path.append(nxt)
                path_pos[nxt] = len(path) - 1

            if in_tree[nxt]:
                break

            current = nxt

        for i in range(len(path) - 1):
            a = path[i]
            b = path[i + 1]
            ar, ac = idx_to_rc(a, cols)
            br, bc = idx_to_rc(b, cols)
            carve_between(maze, ar, ac, br, bc)
            if not in_tree[a]:
                in_tree[a] = True
                remaining -= 1

    return maze


def build_adjacency(maze: Sequence[Sequence[int]]) -> List[List[int]]:
    rows, cols = len(maze), len(maze[0])
    adjacency: List[List[int]] = [[] for _ in range(rows * cols)]

    for r in range(rows):
        for c in range(cols):
            idx = rc_to_idx(r, c, cols)
            cell = maze[r][c]
            if cell & N:
                adjacency[idx].append(rc_to_idx(r - 1, c, cols))
            if cell & E:
                adjacency[idx].append(rc_to_idx(r, c + 1, cols))
            if cell & S:
                adjacency[idx].append(rc_to_idx(r + 1, c, cols))
            if cell & W:
                adjacency[idx].append(rc_to_idx(r, c - 1, cols))

    return adjacency


def shortest_path_steps(adjacency: Sequence[Sequence[int]], start_idx: int, goal_idx: int) -> int:
    dist = [-1 for _ in range(len(adjacency))]
    q: deque[int] = deque([start_idx])
    dist[start_idx] = 0

    while q:
        current = q.popleft()
        if current == goal_idx:
            return dist[current]

        for nxt in adjacency[current]:
            if dist[nxt] == -1:
                dist[nxt] = dist[current] + 1
                q.append(nxt)

    raise RuntimeError("Nao existe caminho entre os extremos do labirinto.")


def random_backtracking_walk(
    adjacency: Sequence[Sequence[int]],
    start_idx: int,
    goal_idx: int,
    rng: random.Random,
    node_visits: Optional[List[int]] = None,
    edge_visits: Optional[Dict[Tuple[int, int], int]] = None,
) -> int:
    n_nodes = len(adjacency)
    visited = [False for _ in range(n_nodes)]

    stack: List[int] = [start_idx]
    visited[start_idx] = True
    if node_visits is not None:
        node_visits[start_idx] += 1

    steps = 0

    while stack:
        current = stack[-1]
        if current == goal_idx:
            return steps

        options = [idx for idx in adjacency[current] if not visited[idx]]

        if options:
            nxt = rng.choice(options)
            visited[nxt] = True
            stack.append(nxt)
            steps += 1

            if node_visits is not None:
                node_visits[nxt] += 1
            if edge_visits is not None:
                edge = (current, nxt) if current < nxt else (nxt, current)
                edge_visits[edge] = edge_visits.get(edge, 0) + 1
        else:
            prev = stack.pop()
            if stack:
                parent = stack[-1]
                steps += 1

                if node_visits is not None:
                    node_visits[parent] += 1
                if edge_visits is not None:
                    edge = (prev, parent) if prev < parent else (parent, prev)
                    edge_visits[edge] = edge_visits.get(edge, 0) + 1

    raise RuntimeError("Falha ao chegar no destino em um labirinto conectado.")


def evaluate_maze(
    maze: Sequence[Sequence[int]],
    rounds_per_direction: int,
    rng: random.Random,
    collect_density: bool = False,
) -> Evaluation:
    rows, cols = len(maze), len(maze[0])
    start_idx = 0
    goal_idx = rows * cols - 1

    adjacency = build_adjacency(maze)
    shortest = shortest_path_steps(adjacency, start_idx, goal_idx)

    results_a_to_b: List[int] = []
    results_b_to_a: List[int] = []

    node_total: Optional[List[int]] = None
    edge_total: Optional[Dict[Tuple[int, int], int]] = None

    if collect_density:
        node_total = [0 for _ in range(rows * cols)]
        edge_total = {}

    for _ in range(rounds_per_direction):
        steps = random_backtracking_walk(
            adjacency,
            start_idx,
            goal_idx,
            rng,
            node_visits=node_total,
            edge_visits=edge_total,
        )
        results_a_to_b.append(steps)

    for _ in range(rounds_per_direction):
        steps = random_backtracking_walk(
            adjacency,
            goal_idx,
            start_idx,
            rng,
            node_visits=node_total,
            edge_visits=edge_total,
        )
        results_b_to_a.append(steps)

    all_steps = results_a_to_b + results_b_to_a
    avg_steps = sum(all_steps) / len(all_steps)
    ratio = avg_steps / shortest if shortest > 0 else float("inf")

    node_avg: Optional[List[int]] = None
    edge_avg: Optional[Dict[Tuple[int, int], int]] = None

    if collect_density and node_total is not None and edge_total is not None:
        total_rounds = 2 * rounds_per_direction
        node_avg = [int(round(v / total_rounds)) for v in node_total]
        edge_avg = {k: int(round(v / total_rounds)) for k, v in edge_total.items()}

    return Evaluation(
        avg_steps=avg_steps,
        difficulty_ratio=ratio,
        shortest=shortest,
        results_a_to_b=results_a_to_b,
        results_b_to_a=results_b_to_a,
        node_visits_avg=node_avg,
        edge_visits_avg=edge_avg,
    )


def better_eval(a: Evaluation, b: Evaluation) -> bool:
    if a.avg_steps != b.avg_steps:
        return a.avg_steps > b.avg_steps
    if a.difficulty_ratio != b.difficulty_ratio:
        return a.difficulty_ratio > b.difficulty_ratio
    return a.shortest > b.shortest


def build_open_mask(maze: Sequence[Sequence[int]]) -> List[List[bool]]:
    rows, cols = len(maze), len(maze[0])
    h = rows * 2 + 1
    w = cols * 2 + 1
    open_mask = [[False for _ in range(w)] for _ in range(h)]

    for r in range(rows):
        for c in range(cols):
            rr, cc = 2 * r + 1, 2 * c + 1
            open_mask[rr][cc] = True
            cell = maze[r][c]
            if cell & N:
                open_mask[rr - 1][cc] = True
            if cell & E:
                open_mask[rr][cc + 1] = True
            if cell & S:
                open_mask[rr + 1][cc] = True
            if cell & W:
                open_mask[rr][cc - 1] = True

    open_mask[0][1] = True
    open_mask[h - 1][w - 2] = True
    return open_mask


def upscale_image(logical: Sequence[Sequence[Tuple[int, int, int]]], scale: int) -> List[List[Tuple[int, int, int]]]:
    out: List[List[Tuple[int, int, int]]] = []
    for row in logical:
        expanded: List[Tuple[int, int, int]] = []
        for color in row:
            expanded.extend([color] * scale)
        for _ in range(scale):
            out.append(expanded.copy())
    return out


def interpolate_color(t: float, low: Tuple[int, int, int], high: Tuple[int, int, int]) -> Tuple[int, int, int]:
    clamped = 0.0 if t < 0.0 else 1.0 if t > 1.0 else t
    return (
        int(round(low[0] + (high[0] - low[0]) * clamped)),
        int(round(low[1] + (high[1] - low[1]) * clamped)),
        int(round(low[2] + (high[2] - low[2]) * clamped)),
    )


def build_maze_image(maze: Sequence[Sequence[int]], scale: int = 8) -> List[List[Tuple[int, int, int]]]:
    rows, cols = len(maze), len(maze[0])
    open_mask = build_open_mask(maze)
    h, w = len(open_mask), len(open_mask[0])

    logical = [[(255, 255, 255) if open_mask[r][c] else (0, 0, 0) for c in range(w)] for r in range(h)]

    start = (0, 0)
    goal = (rows - 1, cols - 1)
    sr, sc = 2 * start[0] + 1, 2 * start[1] + 1
    gr, gc = 2 * goal[0] + 1, 2 * goal[1] + 1

    logical[sr][sc] = (80, 180, 80)
    logical[0][1] = (80, 180, 80)
    logical[gr][gc] = (200, 80, 80)
    logical[h - 1][w - 2] = (200, 80, 80)

    return upscale_image(logical, scale)


def build_density_image(
    maze: Sequence[Sequence[int]],
    node_avg: Sequence[int],
    edge_avg: Dict[Tuple[int, int], int],
    scale: int = 8,
) -> List[List[Tuple[int, int, int]]]:
    rows, cols = len(maze), len(maze[0])
    open_mask = build_open_mask(maze)
    h, w = len(open_mask), len(open_mask[0])

    density_grid: List[List[Optional[int]]] = [[None for _ in range(w)] for _ in range(h)]

    for idx, value in enumerate(node_avg):
        r, c = idx_to_rc(idx, cols)
        density_grid[2 * r + 1][2 * c + 1] = value

    adjacency = build_adjacency(maze)
    for idx, neighbors in enumerate(adjacency):
        r1, c1 = idx_to_rc(idx, cols)
        rr1, cc1 = 2 * r1 + 1, 2 * c1 + 1
        for nxt in neighbors:
            if nxt < idx:
                continue
            r2, c2 = idx_to_rc(nxt, cols)
            rr2, cc2 = 2 * r2 + 1, 2 * c2 + 1
            mid_r = (rr1 + rr2) // 2
            mid_c = (cc1 + cc2) // 2
            edge = (idx, nxt)
            density_grid[mid_r][mid_c] = edge_avg.get(edge, 0)

    start_idx = 0
    goal_idx = rows * cols - 1
    density_grid[0][1] = node_avg[start_idx]
    density_grid[h - 1][w - 2] = node_avg[goal_idx]

    values = [density_grid[r][c] for r in range(h) for c in range(w) if density_grid[r][c] is not None]
    min_v = min(values) if values else 0
    max_v = max(values) if values else 1
    if max_v == min_v:
        max_v = min_v + 1

    low_color = (173, 216, 230)
    high_color = (255, 182, 193)

    logical: List[List[Tuple[int, int, int]]] = []
    for r in range(h):
        line: List[Tuple[int, int, int]] = []
        for c in range(w):
            value = density_grid[r][c]
            if value is None and not open_mask[r][c]:
                line.append((0, 0, 0))
            else:
                v = 0 if value is None else value
                t = (v - min_v) / (max_v - min_v)
                line.append(interpolate_color(t, low_color, high_color))
        logical.append(line)

    return upscale_image(logical, scale)


def png_chunk(chunk_type: bytes, data: bytes) -> bytes:
    length = struct.pack(">I", len(data))
    crc = zlib.crc32(chunk_type + data) & 0xFFFFFFFF
    return length + chunk_type + data + struct.pack(">I", crc)


def write_png_rgb(path: Path, image: Sequence[Sequence[Tuple[int, int, int]]]) -> None:
    height = len(image)
    width = len(image[0]) if height > 0 else 0

    rows = []
    for row in image:
        data = bytearray([0])
        for r, g, b in row:
            data.extend((r, g, b))
        rows.append(bytes(data))

    raw = b"".join(rows)

    content = bytearray(b"\x89PNG\r\n\x1a\n")
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    content.extend(png_chunk(b"IHDR", ihdr))
    content.extend(png_chunk(b"IDAT", zlib.compress(raw, level=9)))
    content.extend(png_chunk(b"IEND", b""))

    path.write_bytes(bytes(content))


def format_difficulty(value: float) -> str:
    return f"{value:.2f}".replace(".", "_")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Busca o melhor metodo para gerar um labirinto 51x51 (25x25 celulas)."
    )
    parser.add_argument("--seed", type=int, default=None, help="Seed global opcional.")
    parser.add_argument(
        "--candidates-per-method",
        type=int,
        default=16,
        help="Quantidade de labirintos testados por metodo na triagem.",
    )
    parser.add_argument(
        "--screen-rounds",
        type=int,
        default=4,
        help="Rodadas por sentido na triagem rapida.",
    )
    parser.add_argument(
        "--final-rounds",
        type=int,
        default=10,
        help="Rodadas por sentido na avaliacao final dos finalistas.",
    )
    parser.add_argument(
        "--top-k",
        type=int,
        default=3,
        help="Finalistas por metodo para reavaliacao detalhada.",
    )
    return parser


def main() -> None:
    args = build_parser().parse_args()

    if args.candidates_per_method <= 0:
        raise ValueError("--candidates-per-method deve ser maior que 0.")
    if args.screen_rounds <= 0 or args.final_rounds <= 0:
        raise ValueError("--screen-rounds e --final-rounds devem ser maiores que 0.")
    if args.top_k <= 0:
        raise ValueError("--top-k deve ser maior que 0.")

    seed = args.seed if args.seed is not None else int(time.time() * 1000) % (2**32)
    master_rng = random.Random(seed)

    print(f"[CONFIG] seed={seed}")
    print("[CONFIG] tamanho fixo: 51x51 (25x25 celulas)")
    print(f"[CONFIG] candidatos por metodo: {args.candidates_per_method}")
    print(f"[CONFIG] triagem: {args.screen_rounds} rodadas por sentido")
    print(f"[CONFIG] final: {args.final_rounds} rodadas por sentido")

    generators: Dict[str, Callable[[int, int, random.Random], List[List[int]]]] = {
        "dfs": generate_dfs,
        "prim": generate_prim,
        "kruskal": generate_kruskal,
        "wilson": generate_wilson,
    }

    best_per_method: Dict[str, Tuple[Candidate, Evaluation]] = {}

    for method_name, generator in generators.items():
        print(f"\n[BUSCA] Metodo: {method_name}")
        top_candidates: List[Candidate] = []

        for i in range(1, args.candidates_per_method + 1):
            maze_seed = master_rng.randrange(2**32)
            gen_rng = random.Random(maze_seed)
            eval_rng = random.Random(maze_seed ^ 0x9E3779B9)

            maze = generator(ROWS, COLS, gen_rng)
            screening = evaluate_maze(
                maze=maze,
                rounds_per_direction=args.screen_rounds,
                rng=eval_rng,
                collect_density=False,
            )

            cand = Candidate(method=method_name, seed=maze_seed, maze=maze, screening=screening)
            top_candidates.append(cand)
            top_candidates.sort(key=lambda c: (c.screening.avg_steps, c.screening.difficulty_ratio, c.screening.shortest), reverse=True)
            if len(top_candidates) > args.top_k:
                top_candidates.pop()

            print(
                f"[BUSCA] {method_name} cand {i:02d}/{args.candidates_per_method} "
                f"avg={screening.avg_steps:.2f} ratio={screening.difficulty_ratio:.2f} "
                f"best={top_candidates[0].screening.avg_steps:.2f}"
            )

        print(f"[FINAL] Reavaliando top {len(top_candidates)} do metodo {method_name}...")
        method_best_candidate = top_candidates[0]
        method_best_eval: Optional[Evaluation] = None

        for rank, cand in enumerate(top_candidates, start=1):
            final_rng = random.Random(cand.seed ^ FINAL_EVAL_XOR)
            final_eval = evaluate_maze(
                maze=cand.maze,
                rounds_per_direction=args.final_rounds,
                rng=final_rng,
                collect_density=False,
            )
            print(
                f"[FINAL] {method_name} top{rank} seed={cand.seed} "
                f"avg={final_eval.avg_steps:.2f} ratio={final_eval.difficulty_ratio:.2f} "
                f"shortest={final_eval.shortest}"
            )

            if method_best_eval is None or better_eval(final_eval, method_best_eval):
                method_best_eval = final_eval
                method_best_candidate = cand

        if method_best_eval is None:
            raise RuntimeError("Falha na reavaliacao final.")

        best_per_method[method_name] = (method_best_candidate, method_best_eval)

    print("\n[RANKING] Melhor candidato por metodo (avaliacao final):")
    ranking = sorted(
        [(m, c, e) for m, (c, e) in best_per_method.items()],
        key=lambda x: (x[2].avg_steps, x[2].difficulty_ratio, x[2].shortest),
        reverse=True,
    )

    for pos, (method_name, cand, ev) in enumerate(ranking, start=1):
        print(
            f"{pos}. metodo={method_name} seed={cand.seed} "
            f"avg={ev.avg_steps:.2f} ratio={ev.difficulty_ratio:.2f} shortest={ev.shortest}"
        )

    winner_method, winner_candidate, winner_eval = ranking[0]

    print("\n[CONFIRMACAO] Reavaliando vencedor com densidade para gerar imagens...")
    density_rng = random.Random(winner_candidate.seed ^ FINAL_EVAL_XOR)
    winner_with_density = evaluate_maze(
        maze=winner_candidate.maze,
        rounds_per_direction=args.final_rounds,
        rng=density_rng,
        collect_density=True,
    )

    if winner_with_density.node_visits_avg is None or winner_with_density.edge_visits_avg is None:
        raise RuntimeError("Falha ao coletar densidade do vencedor.")

    out_dir = Path("outputs")
    out_dir.mkdir(parents=True, exist_ok=True)

    diff_tag = format_difficulty(winner_with_density.difficulty_ratio)
    maze_name = f"best-maze-python-25x25-{winner_method}-{diff_tag}.png"
    test_name = f"best-test-python-25x25-{winner_method}-{diff_tag}.png"

    maze_path = out_dir / maze_name
    test_path = out_dir / test_name

    maze_img = build_maze_image(winner_candidate.maze, scale=8)
    density_img = build_density_image(
        winner_candidate.maze,
        node_avg=winner_with_density.node_visits_avg,
        edge_avg=winner_with_density.edge_visits_avg,
        scale=8,
    )

    write_png_rgb(maze_path, maze_img)
    write_png_rgb(test_path, density_img)

    print("\n[RESULTADO FINAL]")
    print(f"Melhor metodo: {winner_method}")
    print(f"Seed do melhor labirinto: {winner_candidate.seed}")
    print(f"Media de passos (2 x {args.final_rounds} rodadas): {winner_with_density.avg_steps:.2f}")
    print(f"Indice dificuldade (media/minimo): {winner_with_density.difficulty_ratio:.2f}x")
    print(f"Menor caminho: {winner_with_density.shortest}")
    print(f"A->B: {winner_with_density.results_a_to_b}")
    print(f"B->A: {winner_with_density.results_b_to_a}")
    print(f"Maze salvo: {maze_path}")
    print(f"Teste salvo: {test_path}")


if __name__ == "__main__":
    main()
