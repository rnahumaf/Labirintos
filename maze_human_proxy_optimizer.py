#!/usr/bin/env python3
"""
Otimiza metodos de geracao de labirinto usando um simulador de agente humano limitado
como proxy de dificuldade.

Labirinto fixo: 51x51 (25x25 celulas).

Fluxo:
1) Gera candidatos com varios metodos.
2) Faz triagem rapida com simulador humano limitado.
3) Reavalia finalistas com mais rodadas.
4) Escolhe o melhor metodo/candidato e salva imagens em outputs/.
"""

from __future__ import annotations

import argparse
import random
import struct
import time
import zlib
from collections import OrderedDict, deque
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
class HumanAgentConfig:
    junction_memory_capacity: int = 625
    recent_memory: int = 24
    local_depth: int = 2
    local_bfs_budget: int = 28
    forward_bonus: float = 0.35
    backtrack_penalty: float = 0.45
    corridor_forward_bonus: float = 0.70
    untried_exit_bonus: float = 1.10
    unseen_bonus: float = 1.45
    revisit_penalty: float = 0.26
    recent_penalty: float = 0.40
    local_frontier_weight: float = 0.12
    max_steps_factor: int = 8
    corridor_backtrack_chance: float = 0.0


@dataclass
class HumanEvaluation:
    avg_effective_steps: float
    difficulty_score: float
    solve_rate: float
    difficulty_ratio: float
    shortest: int
    results_a_to_b: List[int]
    results_b_to_a: List[int]
    solved_a_to_b: int
    solved_b_to_a: int
    node_visits_avg: Optional[List[int]] = None
    edge_visits_avg: Optional[Dict[Tuple[int, int], int]] = None


@dataclass
class Candidate:
    method: str
    seed: int
    maze: List[List[int]]
    screening: HumanEvaluation


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
        raise ValueError("Celulas nao adjacentes.")


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

    sr, sc = rng.randrange(rows), rng.randrange(cols)
    visited[sr][sc] = True
    frontier: List[Tuple[int, int, int, int]] = []

    def add_frontier(r: int, c: int) -> None:
        for dr, dc, _, _ in DIRECTIONS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and not visited[nr][nc]:
                frontier.append((r, c, nr, nc))

    add_frontier(sr, sc)

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

    raise RuntimeError("Nao ha caminho entre entrada e saida.")


def bounded_local_frontier(
    adjacency: Sequence[Sequence[int]],
    source: int,
    visited_count: Sequence[int],
    depth: int,
    budget: int,
) -> float:
    queue: deque[Tuple[int, int]] = deque([(source, 0)])
    seen = {source}
    score = 0.0
    expanded = 0

    while queue and expanded < budget:
        node, d = queue.popleft()
        if d >= depth:
            continue

        for nxt in adjacency[node]:
            if nxt in seen:
                continue
            seen.add(nxt)
            expanded += 1

            if visited_count[nxt] == 0:
                score += 1.0
            elif visited_count[nxt] == 1:
                score += 0.2

            if len(adjacency[nxt]) >= 3:
                score += 0.15

            queue.append((nxt, d + 1))
            if expanded >= budget:
                break

    return score


def weighted_choice(rng: random.Random, options: Sequence[int], weights: Sequence[float]) -> int:
    total = 0.0
    for w in weights:
        total += w

    threshold = rng.random() * total
    acc = 0.0
    for idx, w in enumerate(weights):
        acc += w
        if acc >= threshold:
            return options[idx]

    return options[-1]


def simulate_limited_human_agent(
    adjacency: Sequence[Sequence[int]],
    start_idx: int,
    goal_idx: int,
    rng: random.Random,
    cfg: HumanAgentConfig,
    node_visits: Optional[List[int]] = None,
    edge_visits: Optional[Dict[Tuple[int, int], int]] = None,
) -> Tuple[int, bool]:
    n_nodes = len(adjacency)
    max_steps = cfg.max_steps_factor * n_nodes

    visited_count = [0 for _ in range(n_nodes)]
    recent_nodes: deque[int] = deque(maxlen=cfg.recent_memory)
    junction_memory: OrderedDict[int, set[int]] = OrderedDict()

    path_stack: List[int] = [start_idx]
    visited_count[start_idx] += 1
    recent_nodes.append(start_idx)
    if node_visits is not None:
        node_visits[start_idx] += 1

    steps = 0

    while steps < max_steps:
        current = path_stack[-1]
        parent = path_stack[-2] if len(path_stack) >= 2 else -1

        if current == goal_idx:
            return steps, True

        neighbors = adjacency[current]
        degree = len(neighbors)

        if current not in junction_memory:
            if len(junction_memory) >= cfg.junction_memory_capacity:
                junction_memory.popitem(last=False)
            junction_memory[current] = set()
        else:
            junction_memory.move_to_end(current)
        tried = junction_memory[current]

        outward = [n for n in neighbors if n != parent]
        untried = [n for n in outward if n not in tried]

        next_node = -1
        if untried:
            options = untried
        else:
            if parent != -1:
                next_node = parent
            else:
                root_untried = [n for n in neighbors if n not in tried]
                options = root_untried if root_untried else list(neighbors)

        if next_node == -1:
            weights: List[float] = []
            for nxt in options:
                score = 1.0
                if parent != -1 and nxt == parent:
                    score -= cfg.backtrack_penalty
                else:
                    score += cfg.forward_bonus
                if degree == 2 and parent != -1 and nxt != parent:
                    score += cfg.corridor_forward_bonus
                if nxt not in tried:
                    score += cfg.untried_exit_bonus
                if visited_count[nxt] == 0:
                    score += cfg.unseen_bonus
                else:
                    score -= cfg.revisit_penalty * visited_count[nxt]
                if nxt in recent_nodes:
                    score -= cfg.recent_penalty

                local = bounded_local_frontier(
                    adjacency=adjacency,
                    source=nxt,
                    visited_count=visited_count,
                    depth=cfg.local_depth,
                    budget=cfg.local_bfs_budget,
                )
                score += cfg.local_frontier_weight * local
                if score < 0.05:
                    score = 0.05
                weights.append(score)

            next_node = weighted_choice(rng, options, weights)

            if current in junction_memory:
                junction_memory[current].add(next_node)

        edge = (current, next_node) if current < next_node else (next_node, current)
        if edge_visits is not None:
            edge_visits[edge] = edge_visits.get(edge, 0) + 1

        if next_node == parent and parent != -1:
            path_stack.pop()
        else:
            path_stack.append(next_node)

        visited_count[next_node] += 1
        recent_nodes.append(next_node)

        if node_visits is not None:
            node_visits[next_node] += 1

        steps += 1

    return steps, False


def evaluate_maze_with_human_proxy(
    maze: Sequence[Sequence[int]],
    rounds_per_direction: int,
    rng: random.Random,
    cfg: HumanAgentConfig,
    collect_density: bool = False,
) -> HumanEvaluation:
    rows, cols = len(maze), len(maze[0])
    start_idx = 0
    goal_idx = rows * cols - 1
    adjacency = build_adjacency(maze)

    shortest = shortest_path_steps(adjacency, start_idx, goal_idx)

    node_total: Optional[List[int]] = None
    edge_total: Optional[Dict[Tuple[int, int], int]] = None
    if collect_density:
        node_total = [0 for _ in range(rows * cols)]
        edge_total = {}

    timeout_penalty = cfg.max_steps_factor * len(adjacency)

    results_a_to_b: List[int] = []
    results_b_to_a: List[int] = []
    solved_a_to_b = 0
    solved_b_to_a = 0

    for _ in range(rounds_per_direction):
        steps, solved = simulate_limited_human_agent(
            adjacency=adjacency,
            start_idx=start_idx,
            goal_idx=goal_idx,
            rng=rng,
            cfg=cfg,
            node_visits=node_total,
            edge_visits=edge_total,
        )
        effective = steps if solved else timeout_penalty
        results_a_to_b.append(effective)
        if solved:
            solved_a_to_b += 1

    for _ in range(rounds_per_direction):
        steps, solved = simulate_limited_human_agent(
            adjacency=adjacency,
            start_idx=goal_idx,
            goal_idx=start_idx,
            rng=rng,
            cfg=cfg,
            node_visits=node_total,
            edge_visits=edge_total,
        )
        effective = steps if solved else timeout_penalty
        results_b_to_a.append(effective)
        if solved:
            solved_b_to_a += 1

    all_results = results_a_to_b + results_b_to_a
    total_rounds = len(all_results)
    solved_total = solved_a_to_b + solved_b_to_a

    avg_effective_steps = sum(all_results) / total_rounds
    solve_rate = solved_total / total_rounds
    difficulty_ratio = avg_effective_steps / shortest if shortest > 0 else float("inf")
    # Score final: passos medios ajustados por falhas (quanto menor solve_rate, maior a dificuldade percebida).
    difficulty_score = avg_effective_steps * (1.0 + (1.0 - solve_rate))

    node_avg: Optional[List[int]] = None
    edge_avg: Optional[Dict[Tuple[int, int], int]] = None
    if collect_density and node_total is not None and edge_total is not None:
        node_avg = [int(round(v / total_rounds)) for v in node_total]
        edge_avg = {k: int(round(v / total_rounds)) for k, v in edge_total.items()}

    return HumanEvaluation(
        avg_effective_steps=avg_effective_steps,
        difficulty_score=difficulty_score,
        solve_rate=solve_rate,
        difficulty_ratio=difficulty_ratio,
        shortest=shortest,
        results_a_to_b=results_a_to_b,
        results_b_to_a=results_b_to_a,
        solved_a_to_b=solved_a_to_b,
        solved_b_to_a=solved_b_to_a,
        node_visits_avg=node_avg,
        edge_visits_avg=edge_avg,
    )


def better_eval(a: HumanEvaluation, b: HumanEvaluation) -> bool:
    if a.difficulty_score != b.difficulty_score:
        return a.difficulty_score > b.difficulty_score
    if a.avg_effective_steps != b.avg_effective_steps:
        return a.avg_effective_steps > b.avg_effective_steps
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

    sr, sc = 1, 1
    gr, gc = 2 * (rows - 1) + 1, 2 * (cols - 1) + 1
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
            density_grid[mid_r][mid_c] = edge_avg.get((idx, nxt), 0)

    density_grid[0][1] = node_avg[0]
    density_grid[h - 1][w - 2] = node_avg[rows * cols - 1]

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


def format_tag(value: float) -> str:
    return f"{value:.2f}".replace(".", "_")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Otimiza metodos de geracao com simulador de agente humano limitado."
    )
    parser.add_argument("--seed", type=int, default=None, help="Seed global opcional.")
    parser.add_argument("--candidates-per-method", type=int, default=16, help="Candidatos por metodo na triagem.")
    parser.add_argument("--screen-rounds", type=int, default=4, help="Rodadas por sentido na triagem.")
    parser.add_argument("--final-rounds", type=int, default=10, help="Rodadas por sentido na final.")
    parser.add_argument("--top-k", type=int, default=3, help="Finalistas por metodo.")
    return parser


def main() -> None:
    args = build_parser().parse_args()

    if args.candidates_per_method <= 0:
        raise ValueError("--candidates-per-method deve ser > 0")
    if args.screen_rounds <= 0 or args.final_rounds <= 0:
        raise ValueError("--screen-rounds e --final-rounds devem ser > 0")
    if args.top_k <= 0:
        raise ValueError("--top-k deve ser > 0")

    cfg = HumanAgentConfig()

    seed = args.seed if args.seed is not None else int(time.time() * 1000) % (2**32)
    master_rng = random.Random(seed)

    print(f"[CONFIG] seed={seed}")
    print("[CONFIG] tamanho fixo: 51x51 (25x25 celulas)")
    print(f"[CONFIG] candidatos por metodo: {args.candidates_per_method}")
    print(f"[CONFIG] triagem: {args.screen_rounds} rodadas por sentido")
    print(f"[CONFIG] final: {args.final_rounds} rodadas por sentido")
    print(
        "[CONFIG] agente humano: "
        f"mem_junc={cfg.junction_memory_capacity}, mem_recente={cfg.recent_memory}, "
        f"visao={cfg.local_depth}, max_steps_factor={cfg.max_steps_factor}"
    )

    generators: Dict[str, Callable[[int, int, random.Random], List[List[int]]]] = {
        "dfs": generate_dfs,
        "prim": generate_prim,
        "kruskal": generate_kruskal,
        "wilson": generate_wilson,
    }

    best_per_method: Dict[str, Tuple[Candidate, HumanEvaluation]] = {}

    for method_name, generator in generators.items():
        print(f"\n[BUSCA] Metodo: {method_name}")
        top_candidates: List[Candidate] = []

        for i in range(1, args.candidates_per_method + 1):
            maze_seed = master_rng.randrange(2**32)
            gen_rng = random.Random(maze_seed)
            eval_rng = random.Random(maze_seed ^ 0x9E3779B9)

            maze = generator(ROWS, COLS, gen_rng)
            screening = evaluate_maze_with_human_proxy(
                maze=maze,
                rounds_per_direction=args.screen_rounds,
                rng=eval_rng,
                cfg=cfg,
                collect_density=False,
            )

            cand = Candidate(method=method_name, seed=maze_seed, maze=maze, screening=screening)
            top_candidates.append(cand)
            top_candidates.sort(
                key=lambda c: (
                    c.screening.difficulty_score,
                    c.screening.avg_effective_steps,
                    c.screening.difficulty_ratio,
                ),
                reverse=True,
            )
            if len(top_candidates) > args.top_k:
                top_candidates.pop()

            best_now = top_candidates[0].screening
            print(
                f"[BUSCA] {method_name} cand {i:02d}/{args.candidates_per_method} "
                f"score={screening.difficulty_score:.2f} avg={screening.avg_effective_steps:.2f} "
                f"solve={screening.solve_rate:.2f} best={best_now.difficulty_score:.2f}"
            )

        print(f"[FINAL] Reavaliando top {len(top_candidates)} de {method_name}...")
        method_best_candidate = top_candidates[0]
        method_best_eval: Optional[HumanEvaluation] = None

        for rank, cand in enumerate(top_candidates, start=1):
            final_rng = random.Random(cand.seed ^ FINAL_EVAL_XOR)
            final_eval = evaluate_maze_with_human_proxy(
                maze=cand.maze,
                rounds_per_direction=args.final_rounds,
                rng=final_rng,
                cfg=cfg,
                collect_density=False,
            )

            print(
                f"[FINAL] {method_name} top{rank} seed={cand.seed} "
                f"score={final_eval.difficulty_score:.2f} avg={final_eval.avg_effective_steps:.2f} "
                f"solve={final_eval.solve_rate:.2f} ratio={final_eval.difficulty_ratio:.2f}"
            )

            if method_best_eval is None or better_eval(final_eval, method_best_eval):
                method_best_eval = final_eval
                method_best_candidate = cand

        if method_best_eval is None:
            raise RuntimeError("Falha na fase final.")

        best_per_method[method_name] = (method_best_candidate, method_best_eval)

    ranking = sorted(
        [(method, cand, ev) for method, (cand, ev) in best_per_method.items()],
        key=lambda x: (x[2].difficulty_score, x[2].avg_effective_steps, x[2].difficulty_ratio),
        reverse=True,
    )

    print("\n[RANKING] Melhor por metodo (proxy humano limitado):")
    for pos, (method_name, cand, ev) in enumerate(ranking, start=1):
        print(
            f"{pos}. metodo={method_name} seed={cand.seed} "
            f"score={ev.difficulty_score:.2f} avg={ev.avg_effective_steps:.2f} "
            f"solve={ev.solve_rate:.2f} ratio={ev.difficulty_ratio:.2f} shortest={ev.shortest}"
        )

    winner_method, winner_candidate, _ = ranking[0]

    print("\n[CONFIRMACAO] Reavaliando vencedor com densidade...")
    density_rng = random.Random(winner_candidate.seed ^ FINAL_EVAL_XOR)
    winner_eval = evaluate_maze_with_human_proxy(
        maze=winner_candidate.maze,
        rounds_per_direction=args.final_rounds,
        rng=density_rng,
        cfg=cfg,
        collect_density=True,
    )

    if winner_eval.node_visits_avg is None or winner_eval.edge_visits_avg is None:
        raise RuntimeError("Falha ao coletar densidade do vencedor.")

    out_dir = Path("outputs")
    out_dir.mkdir(parents=True, exist_ok=True)

    score_tag = format_tag(winner_eval.difficulty_score)
    maze_path = out_dir / f"human-best-maze-python-25x25-{winner_method}-{score_tag}.png"
    test_path = out_dir / f"human-best-test-python-25x25-{winner_method}-{score_tag}.png"

    maze_img = build_maze_image(winner_candidate.maze, scale=8)
    density_img = build_density_image(
        winner_candidate.maze,
        node_avg=winner_eval.node_visits_avg,
        edge_avg=winner_eval.edge_visits_avg,
        scale=8,
    )

    write_png_rgb(maze_path, maze_img)
    write_png_rgb(test_path, density_img)

    print("\n[RESULTADO FINAL]")
    print(f"Melhor metodo: {winner_method}")
    print(f"Seed do melhor labirinto: {winner_candidate.seed}")
    print(f"Score proxy humano: {winner_eval.difficulty_score:.2f}")
    print(f"Media de passos efetivos: {winner_eval.avg_effective_steps:.2f}")
    print(f"Taxa de solucao: {winner_eval.solve_rate:.2f}")
    print(f"Indice media/minimo: {winner_eval.difficulty_ratio:.2f}x")
    print(f"Menor caminho: {winner_eval.shortest}")
    print(f"A->B: {winner_eval.results_a_to_b} | resolvidos: {winner_eval.solved_a_to_b}/{args.final_rounds}")
    print(f"B->A: {winner_eval.results_b_to_a} | resolvidos: {winner_eval.solved_b_to_a}/{args.final_rounds}")
    print(f"Maze salvo: {maze_path}")
    print(f"Teste salvo: {test_path}")


if __name__ == "__main__":
    main()
