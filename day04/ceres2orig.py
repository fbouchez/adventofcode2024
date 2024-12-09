#!/usr/bin/env python3

# Lire la grille depuis un fichier
with open("input-mine.txt", "r") as file:
    grid = [line.strip() for line in file.readlines()]

# Dimensions de la grille
rows, cols = len(grid), len(grid[0])

# Fonction pour vérifier si une "X-MAS" existe avec un centre donné
def check_x_mas(grid, x, y):
    # Vérifier les diagonales pour "MAS" ou "SAM"
    if (
        x - 1 >= 0 and x + 1 < rows and y - 1 >= 0 and y + 1 < cols and  # Limites
        ((grid[x - 1][y - 1] == "M" and grid[x + 1][y + 1] == "S" and grid[x][y] == "A") or
         (grid[x - 1][y - 1] == "S" and grid[x + 1][y + 1] == "M" and grid[x][y] == "A")) and
        ((grid[x - 1][y + 1] == "M" and grid[x + 1][y - 1] == "S" and grid[x][y] == "A") or
         (grid[x - 1][y + 1] == "S" and grid[x + 1][y - 1] == "M" and grid[x][y] == "A"))
    ):
        print ("match at", x, y)
        return True
    return False

# Compter les occurrences de X-MAS
count = 0
for x in range(rows):
    for y in range(cols):
        if check_x_mas(grid, x, y):
            count += 1

# Afficher le résultat
print(count)
