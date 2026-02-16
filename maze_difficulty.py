#!/usr/bin/env python3
"""
Gera um labirinto aleatorio e estima dificuldade com caminhadas randomicas com backtracking.

Regras principais:
- Gera o labirinto por caminhada aleatoria em celulas ainda nao visitadas
  (evita iteracoes redundantes de tentativas invalidas).
- Entrada fixa no topo-esquerda e saida fixa na base-direita.
- Executa N rodadas de caminhada de teste da entrada ate a saida.
- Mostra progresso da geracao e das rodadas no terminal.
"""

from __future__ import annotations

import argparse
import random
import struct
import time
import zlib
from collections import deque
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

# Bitmasks para passagens entre celulas
N, E, S, W = 1, 2, 4, 8
DIRECTIONS: Sequence[Tuple[int, int, int, int]] = (
    (-1, 0, N, S),
    (0, 1, E, W),
    (1, 0, S, N),
    (0, -1, W, E),
)


def generate_maze(rows: int, cols: int, rng: random.Random, progress_step: float = 0.10) -> List[List[int]]:
    """Gera um labirinto perfeito usando DFS iterativo aleatorio."""
    maze = [[0 for _ in range(cols)] for _ in range(rows)]
    visited = [[False for _ in range(cols)] for _ in range(rows)]

    stack: List[Tuple[int, int]] = [(0, 0)]
    visited[0][0] = True

    total = rows * cols
    visited_count = 1
    next_report = progress_step

    print(f"[GERACAO] Inicio: {rows}x{cols} celulas ({total} no total).")

    while stack:
        r, c = stack[-1]

        candidates = []
        for dr, dc, bit, opposite_bit in DIRECTIONS:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and not visited[nr][nc]:
                candidates.append((nr, nc, bit, opposite_bit))

        if not candidates:
            stack.pop()
            continue

        nr, nc, bit, opposite_bit = rng.choice(candidates)
        maze[r][c] |= bit
        maze[nr][nc] |= opposite_bit

        visited[nr][nc] = True
        visited_count += 1
        stack.append((nr, nc))

        progress = visited_count / total
        if progress >= next_report:
            print(f"[GERACAO] Progresso: {progress * 100:5.1f}% ({visited_count}/{total})")
            next_report += progress_step

    print("[GERACAO] Progresso: 100.0% (labirinto concluido)")
    return maze


def iter_neighbors(maze: Sequence[Sequence[int]], r: int, c: int):
    cell = maze[r][c]
    if cell & N:
        yield r - 1, c
    if cell & E:
        yield r, c + 1
    if cell & S:
        yield r + 1, c
    if cell & W:
        yield r, c - 1


def rc_to_idx(r: int, c: int, cols: int) -> int:
    return r * cols + c


def build_adjacency(maze: Sequence[Sequence[int]]) -> List[List[int]]:
    rows, cols = len(maze), len(maze[0])
    adjacency: List[List[int]] = [[] for _ in range(rows * cols)]

    for r in range(rows):
        for c in range(cols):
            idx = rc_to_idx(r, c, cols)
            neighbors_idx = [rc_to_idx(nr, nc, cols) for nr, nc in iter_neighbors(maze, r, c)]
            adjacency[idx] = neighbors_idx

    return adjacency


def random_backtracking_walk(
    adjacency: Sequence[Sequence[int]],
    start_idx: int,
    goal_idx: int,
    rng: random.Random,
    walk_label: str,
    progress_every: int,
    node_visits: Optional[List[int]] = None,
    edge_visits: Optional[Dict[Tuple[int, int], int]] = None,
) -> int:
    """Simula tentativa humana: explora caminho aleatorio, faz backtracking em becos sem saida."""
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

        unvisited_neighbors = [idx for idx in adjacency[current] if not visited[idx]]

        if unvisited_neighbors:
            nxt = rng.choice(unvisited_neighbors)
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

        if progress_every > 0 and steps > 0 and steps % progress_every == 0:
            print(f"[TESTE {walk_label}] passos acumulados: {steps}")

    raise RuntimeError("Nao foi possivel chegar a saida; revise a geracao do labirinto.")


def shortest_path_steps(
    adjacency: Sequence[Sequence[int]],
    start_idx: int,
    goal_idx: int,
) -> int:
    """Numero minimo de passos entre entrada e saida (BFS)."""
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

    raise RuntimeError("Nao existe caminho da entrada para a saida.")


def render_ascii(
    maze: Sequence[Sequence[int]],
    start: Tuple[int, int],
    goal: Tuple[int, int],
) -> str:
    """Converte o labirinto para ASCII com entrada no topo-esquerda e saida na base-direita."""
    rows, cols = len(maze), len(maze[0])
    h = rows * 2 + 1
    w = cols * 2 + 1

    canvas = [["#" for _ in range(w)] for _ in range(h)]

    for r in range(rows):
        for c in range(cols):
            rr, cc = 2 * r + 1, 2 * c + 1
            canvas[rr][cc] = " "
            cell = maze[r][c]
            if cell & N:
                canvas[rr - 1][cc] = " "
            if cell & E:
                canvas[rr][cc + 1] = " "
            if cell & S:
                canvas[rr + 1][cc] = " "
            if cell & W:
                canvas[rr][cc - 1] = " "

    # Entrada (topo-esquerda) e saida (base-direita)
    canvas[0][1] = " "
    canvas[h - 1][w - 2] = " "

    sr, sc = 2 * start[0] + 1, 2 * start[1] + 1
    gr, gc = 2 * goal[0] + 1, 2 * goal[1] + 1
    canvas[sr][sc] = "S"
    canvas[gr][gc] = "E"

    return "\n".join("".join(row) for row in canvas)


def build_open_mask(
    maze: Sequence[Sequence[int]],
) -> List[List[bool]]:
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

    # Aberturas externas: entrada e saida
    open_mask[0][1] = True
    open_mask[h - 1][w - 2] = True
    return open_mask


def upscale_image(
    logical: Sequence[Sequence[Tuple[int, int, int]]],
    scale: int,
) -> List[List[Tuple[int, int, int]]]:
    out: List[List[Tuple[int, int, int]]] = []
    for row in logical:
        expanded_row: List[Tuple[int, int, int]] = []
        for color in row:
            expanded_row.extend([color] * scale)
        for _ in range(scale):
            out.append(expanded_row.copy())
    return out


def interpolate_color(
    t: float,
    low: Tuple[int, int, int],
    high: Tuple[int, int, int],
) -> Tuple[int, int, int]:
    clamped = 0.0 if t < 0.0 else 1.0 if t > 1.0 else t
    return (
        int(round(low[0] + (high[0] - low[0]) * clamped)),
        int(round(low[1] + (high[1] - low[1]) * clamped)),
        int(round(low[2] + (high[2] - low[2]) * clamped)),
    )


def build_maze_image(
    maze: Sequence[Sequence[int]],
    start: Tuple[int, int],
    goal: Tuple[int, int],
    scale: int = 8,
) -> List[List[Tuple[int, int, int]]]:
    open_mask = build_open_mask(maze)
    h, w = len(open_mask), len(open_mask[0])
    logical = [[(255, 255, 255) if open_mask[r][c] else (0, 0, 0) for c in range(w)] for r in range(h)]

    sr, sc = 2 * start[0] + 1, 2 * start[1] + 1
    gr, gc = 2 * goal[0] + 1, 2 * goal[1] + 1
    logical[sr][sc] = (80, 180, 80)
    logical[0][1] = (80, 180, 80)
    logical[gr][gc] = (200, 80, 80)
    logical[h - 1][w - 2] = (200, 80, 80)

    return upscale_image(logical, scale)


def build_density_image(
    maze: Sequence[Sequence[int]],
    adjacency: Sequence[Sequence[int]],
    rows: int,
    cols: int,
    start_idx: int,
    goal_idx: int,
    avg_node: Sequence[int],
    avg_edge: Dict[Tuple[int, int], int],
    scale: int = 8,
) -> List[List[Tuple[int, int, int]]]:
    open_mask = build_open_mask(maze)
    h, w = len(open_mask), len(open_mask[0])

    density_grid: List[List[Optional[int]]] = [[None for _ in range(w)] for _ in range(h)]

    for idx, value in enumerate(avg_node):
        r, c = divmod(idx, cols)
        rr, cc = 2 * r + 1, 2 * c + 1
        density_grid[rr][cc] = value

    for idx, neighbors in enumerate(adjacency):
        r1, c1 = divmod(idx, cols)
        rr1, cc1 = 2 * r1 + 1, 2 * c1 + 1
        for nxt in neighbors:
            if nxt < idx:
                continue
            r2, c2 = divmod(nxt, cols)
            rr2, cc2 = 2 * r2 + 1, 2 * c2 + 1
            mid_r = (rr1 + rr2) // 2
            mid_c = (cc1 + cc2) // 2
            edge = (idx, nxt) if idx < nxt else (nxt, idx)
            density_grid[mid_r][mid_c] = avg_edge.get(edge, 0)

    start_r, start_c = divmod(start_idx, cols)
    goal_r, goal_c = divmod(goal_idx, cols)
    density_grid[0][1] = avg_node[start_idx]
    density_grid[h - 1][w - 2] = avg_node[goal_idx]
    density_grid[2 * start_r + 1][2 * start_c + 1] = avg_node[start_idx]
    density_grid[2 * goal_r + 1][2 * goal_c + 1] = avg_node[goal_idx]

    all_values = [density_grid[r][c] for r in range(h) for c in range(w) if density_grid[r][c] is not None]
    min_v = min(all_values) if all_values else 0
    max_v = max(all_values) if all_values else 1
    if max_v == min_v:
        max_v = min_v + 1

    low_color = (173, 216, 230)  # azul claro
    high_color = (255, 182, 193)  # vermelho claro

    logical: List[List[Tuple[int, int, int]]] = []
    for r in range(h):
        row_colors: List[Tuple[int, int, int]] = []
        for c in range(w):
            value = density_grid[r][c]
            if value is None and not open_mask[r][c]:
                row_colors.append((0, 0, 0))
            else:
                v = 0 if value is None else value
                t = (v - min_v) / (max_v - min_v)
                row_colors.append(interpolate_color(t, low_color, high_color))
        logical.append(row_colors)

    return upscale_image(logical, scale)


def png_chunk(chunk_type: bytes, data: bytes) -> bytes:
    length = struct.pack(">I", len(data))
    crc = zlib.crc32(chunk_type + data) & 0xFFFFFFFF
    return length + chunk_type + data + struct.pack(">I", crc)


def write_png_rgb(path: Path, image: Sequence[Sequence[Tuple[int, int, int]]]) -> None:
    height = len(image)
    width = len(image[0]) if height > 0 else 0

    raw_rows = []
    for row in image:
        row_bytes = bytearray()
        row_bytes.append(0)  # filtro PNG: None
        for r, g, b in row:
            row_bytes.extend((r, g, b))
        raw_rows.append(bytes(row_bytes))
    raw_data = b"".join(raw_rows)

    png_sig = b"\x89PNG\r\n\x1a\n"
    ihdr = struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)
    idat = zlib.compress(raw_data, level=9)

    content = bytearray()
    content.extend(png_sig)
    content.extend(png_chunk(b"IHDR", ihdr))
    content.extend(png_chunk(b"IDAT", idat))
    content.extend(png_chunk(b"IEND", b""))

    path.write_bytes(bytes(content))


def format_difficulty_tag(value: float) -> str:
    return f"{value:.2f}".replace(".", "_")


def positive_odd(value: str) -> int:
    ivalue = int(value)
    if ivalue <= 2 or ivalue % 2 == 0:
        raise argparse.ArgumentTypeError("Use um inteiro impar maior que 2.")
    return ivalue


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Gera labirinto aleatorio e estima dificuldade por media de passos.",
    )
    parser.add_argument("--width", type=positive_odd, default=51, help="Largura da matriz final (impar).")
    parser.add_argument("--height", type=positive_odd, default=51, help="Altura da matriz final (impar).")
    parser.add_argument(
        "--rounds",
        type=int,
        default=10,
        help="Numero de rodadas por sentido (A->B e B->A).",
    )
    parser.add_argument("--seed", type=int, default=None, help="Seed opcional para reproducao.")
    parser.add_argument("--show-maze", action="store_true", help="Imprime o labirinto em ASCII ao final.")
    return parser


def main() -> None:
    args = build_parser().parse_args()

    if args.rounds <= 0:
        raise ValueError("--rounds deve ser maior que 0.")

    seed = args.seed if args.seed is not None else int(time.time() * 1000) % (2**32)
    rng = random.Random(seed)

    rows = (args.height - 1) // 2
    cols = (args.width - 1) // 2
    start = (0, 0)
    goal = (rows - 1, cols - 1)

    print(f"[CONFIG] seed={seed}")
    print(f"[CONFIG] matriz={args.height}x{args.width} | celulas={rows}x{cols}")
    print("[CONFIG] entrada=topo-esquerda | saida=base-direita")

    maze = generate_maze(rows=rows, cols=cols, rng=rng)
    adjacency = build_adjacency(maze)
    start_idx = rc_to_idx(start[0], start[1], cols)
    goal_idx = rc_to_idx(goal[0], goal[1], cols)

    shortest = shortest_path_steps(adjacency, start_idx, goal_idx)
    print(f"[DIAGNOSTICO] menor caminho possivel: {shortest} passos")

    results: List[int] = []
    results_a_to_b: List[int] = []
    results_b_to_a: List[int] = []
    node_visits_total = [0 for _ in range(rows * cols)]
    edge_visits_total: Dict[Tuple[int, int], int] = {}
    progress_every = max(200, (rows * cols) // 3)

    for i in range(1, args.rounds + 1):
        print(f"[TESTE A->{i:02d}] iniciando rodada...")
        steps = random_backtracking_walk(
            adjacency=adjacency,
            start_idx=start_idx,
            goal_idx=goal_idx,
            rng=rng,
            walk_label=f"A->{i:02d}",
            progress_every=progress_every,
            node_visits=node_visits_total,
            edge_visits=edge_visits_total,
        )
        results.append(steps)
        results_a_to_b.append(steps)
        print(f"[TESTE A->{i:02d}] concluida com {steps} passos")

    for i in range(1, args.rounds + 1):
        print(f"[TESTE B->{i:02d}] iniciando rodada...")
        steps = random_backtracking_walk(
            adjacency=adjacency,
            start_idx=goal_idx,
            goal_idx=start_idx,
            rng=rng,
            walk_label=f"B->{i:02d}",
            progress_every=progress_every,
            node_visits=node_visits_total,
            edge_visits=edge_visits_total,
        )
        results.append(steps)
        results_b_to_a.append(steps)
        print(f"[TESTE B->{i:02d}] concluida com {steps} passos")

    avg_steps = sum(results) / len(results)
    difficulty_ratio = avg_steps / shortest if shortest > 0 else float("inf")
    total_rounds = len(results)
    avg_node = [int(round(v / total_rounds)) for v in node_visits_total]
    avg_edge = {k: int(round(v / total_rounds)) for k, v in edge_visits_total.items()}

    difficulty_tag = format_difficulty_tag(difficulty_ratio)
    language_tag = "python"
    cells_tag = f"{rows}x{cols}"
    out_dir = Path("outputs")
    out_dir.mkdir(parents=True, exist_ok=True)

    maze_filename = f"maze-{language_tag}-{cells_tag}-{difficulty_tag}.png"
    test_filename = f"test-{language_tag}-{cells_tag}-{difficulty_tag}.png"
    maze_path = out_dir / maze_filename
    test_path = out_dir / test_filename

    print("[ARQUIVOS] gerando imagens em outputs/ ...")
    maze_img = build_maze_image(maze=maze, start=start, goal=goal, scale=8)
    write_png_rgb(maze_path, maze_img)

    density_img = build_density_image(
        maze=maze,
        adjacency=adjacency,
        rows=rows,
        cols=cols,
        start_idx=start_idx,
        goal_idx=goal_idx,
        avg_node=avg_node,
        avg_edge=avg_edge,
        scale=8,
    )
    write_png_rgb(test_path, density_img)

    print("\n[RESUMO]")
    print(f"Rodadas A->B: {args.rounds}")
    print(f"Rodadas B->A: {args.rounds}")
    print(f"Rodadas totais: {total_rounds}")
    print(f"Passos A->B: {results_a_to_b}")
    print(f"Passos B->A: {results_b_to_a}")
    print(f"Media de passos: {avg_steps:.2f}")
    print(f"Indice de dificuldade (media/minimo): {difficulty_ratio:.2f}x")
    print(f"Maze salvo: {maze_path}")
    print(f"Teste salvo: {test_path}")

    if args.show_maze:
        print("\n[LABIRINTO ASCII]")
        print(render_ascii(maze, start, goal))


if __name__ == "__main__":
    main()
