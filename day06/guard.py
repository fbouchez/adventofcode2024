#!/usr/bin/env python3
import sys

def simulate_guard_path(grid):
    directions = {'^': (-1, 0), '>': (0, 1), 'v': (1, 0), '<': (0, -1)}
    turn_right = {'^': '>',
                  '>': 'v',
                  'v': '<',
                  '<': '^'}
    
    # Find the guard's starting position and orientation
    rows, cols = len(grid), len(grid[0])
    guard_pos = None
    guard_dir = None
    
    for r in range(grid):
        assert len(grid[r]) == cols
        for c in range(cols):
            if grid[r][c] in directions:
                guard_pos = (r, c)
                guard_dir = grid[r][c]
                break
        if guard_pos:
            break
    
    # Track visited positions and simulate the guard's movement
    visited = set()
    visited.add(guard_pos)
    
    while True:
        # Calculate the next position
        dr, dc = directions[guard_dir]
        next_pos = (guard_pos[0] + dr, guard_pos[1] + dc)
        
        # Check if the next position is outside the grid
        if not (0 <= next_pos[0] < rows and 0 <= next_pos[1] < cols):
            break
        
        # Check if the next position is an obstacle
        if grid[next_pos[0]][next_pos[1]] == '#':
            # Turn right if there's an obstacle
            guard_dir = turn_right[guard_dir]
        else:
            # Move forward if the next position is free
            guard_pos = next_pos
            visited.add(guard_pos)
    
    return len(visited)

# Read input from stdin
if __name__ == "__main__":
    grid = [list(line.strip()) for line in sys.stdin]
    result = simulate_guard_path(grid)
    print(f"The guard visits {result} distinct positions.")
