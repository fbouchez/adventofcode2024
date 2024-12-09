
import sys
from collections import defaultdict

def parse_input():
    """Parse l'entrée de la carte depuis stdin."""
    return [list(line.strip()) for line in sys.stdin]

def move_guard(grid, pos, direction):
    """Fait avancer le garde d'une étape selon les règles."""
    directions = {'^': (-1, 0), 'v': (1, 0), '<': (0, -1), '>': (0, 1)}
    right_turn = {'^': '>', '>': 'v', 'v': '<', '<': '^'}

    x, y = pos
    dx, dy = directions[direction]
    next_pos = (x + dx, y + dy)

    if 0 <= next_pos[0] < len(grid) and 0 <= next_pos[1] < len(grid[0]) and grid[next_pos[0]][next_pos[1]] != '#':
        # Avancer si possible
        return next_pos, direction
    else:
        # Tourner à droite sinon
        return pos, right_turn[direction]

def simulate(grid, start_pos, start_direction):
    """Simule le chemin du garde et détecte une boucle."""
    visited = set()
    pos, direction = start_pos, start_direction

    while True:
        state = (pos, direction)
        if state in visited:
            # Boucle détectée
            return True
        visited.add(state)

        pos, direction = move_guard(grid, pos, direction)

        # Si le garde sort de la carte
        x, y = pos
        if not (0 <= x < len(grid) and 0 <= y < len(grid[0])):
            return False

def count_valid_obstacle_positions(grid):
    """Compte les positions où un obstacle peut être placé pour bloquer le garde dans une boucle."""
    # Trouver la position de départ et la direction initiale du garde
    directions = {'^', 'v', '<', '>'}
    start_pos = None
    start_direction = None

    for x, row in enumerate(grid):
        for y, cell in enumerate(row):
            if cell in directions:
                start_pos = (x, y)
                start_direction = cell
                break
        if start_pos:
            break

    valid_positions = 0

    pos_counter = 0
    max_pos = len(grid) * len(grid[0])
    # Tester chaque position libre pour placer un obstacle
    for x, row in enumerate(grid):
        for y, cell in enumerate(row):
            if cell == '.' and (x, y) != start_pos:

                pos_counter += 1
                print(f"Test position {pos_counter} sur max {max_pos}")
                # Placer un obstacle temporaire
                grid[x][y] = '#'
                if simulate(grid, start_pos, start_direction):
                    print("valide")
                    valid_positions += 1
                # Enlever l'obstacle temporaire
                grid[x][y] = '.'

    return valid_positions

def main():
    grid = parse_input()
    result = count_valid_obstacle_positions(grid)
    print(result)

if __name__ == "__main__":
    main()
