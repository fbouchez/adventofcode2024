
import sys

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

    if not (0 <= next_pos[0] < len(grid) and 0 <= next_pos[1] < len(grid[0])):
        # sortie de carte
        return None, None

    if grid[next_pos[0]][next_pos[1]] != '#':
        # Avancer si possible
        return next_pos, direction
    else:
        # Tourner à droite sinon
        return pos, right_turn[direction]

def simulate_path(grid, start_pos, start_direction):
    """Simule le chemin du garde et retourne les positions visitées."""
    visited = {}
    pos, direction = start_pos, start_direction

    loop = False
    while True:
        if pos not in visited:
            visited[pos] = direction

        else:
            x,y = pos
            old_dirs = visited[(x,y)]
            if direction in old_dirs:
                # boucle detectée
                loop = True
                break

            # Si la position est déjà visitée, rajouter notre direction 
            # actuelle
            visited[(x, y)] = old_dirs + direction


        pos, direction = move_guard(grid, pos, direction)

        # Si le garde sort de la carte
        if pos is None:
            break

    return loop, visited


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

    # Obtenir le chemin visité par le garde
    loop, visited_positions = simulate_path(grid, start_pos, start_direction)

    if loop:
        print("gros probleme")
        display_guard_path(grid, visited_positions)

    assert not loop


    valid_positions = 0

    # Tester uniquement les positions du chemin visité
    for pos in visited_positions:
        if pos == start_pos:  # Ne pas placer l'obstacle à la position de départ
            continue
            # if simulate_with_obstacle(grid, start_pos, start_direction, pos):
        print()

        r,c = pos
        assert grid[r][c] == '.'
        grid[r][c] = '#'
        loop, visited_obs = simulate_path(grid, start_pos, start_direction)

        print(f"Test {r}x{c}: {loop}")

        # if display_guard_path(grid, start_pos, start_direction, pos):
            # print(f"valide {pos}")
        if loop:
            display_guard_path(grid, visited_obs, pos)
            valid_positions += 1
        grid[r][c] = '.'

    return valid_positions


def display_guard_path(grid, visited_positions, obstacle=None):
    """Affiche la carte avec le chemin du garde et un obstacle."""
    directions = {'^': (-1, 0), 'v': (1, 0), '<': (0, -1), '>': (0, 1)}
    right_turn = {'^': '>', '>': 'v', 'v': '<', '<': '^'}

    # Copie la carte pour modification
    display_grid = [list(row) for row in grid]

    # Ajouter l'obstacle
    if obstacle:
        x, y = obstacle
        display_grid[x][y] = 'O'


    # Appliquer les chemins visités à la carte
    for (x, y), symbols in visited_positions.items():
        car = '.'
        if '^' in symbols or 'v' in symbols:
            car = '|'
        if '<' in symbols or '>' in symbols:
            if car == '|':
                car = '+'
            else:
                car = '-'
        display_grid[x][y] = car

    # Convertir la carte en texte et l'afficher
    for row in display_grid:
        print(''.join(row))


def main():
    grid = parse_input()
    result = count_valid_obstacle_positions(grid)
    print(result)

if __name__ == "__main__":
    main()
