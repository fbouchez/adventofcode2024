#!/usr/bin/env python3

# Lire la grille depuis un fichier
# with open("input-small.txt", "r") as file:
# with open("input-small2.txt", "r") as file:
with open("input-mine.txt", "r") as file:
    grid = [line.strip() for line in file.readlines()]

# Dimensions de la grille
rows, cols = len(grid), len(grid[1])


def verify(a,b):
    return a == 'M' and b == 'S' or a == 'S' and b == 'M'


# Fonction pour vérifier si une "X-MAS" existe avec un centre donné
def check_x_mas(grid, x, y):
    # Vérifier les diagonales pour "MAS" ou "SAM"
    d1 = [grid[x - 1][y - 1], grid[x + 1][y + 1]]
    d2 = [grid[x + 1][y - 1], grid[x - 1][y + 1]]

    d1.sort()
    d2.sort()
    ref = ["M","S"]

    match = d1 == ref and d2 == ref
    return match

# Compter les occurrences de X-MAS
count = 0
for x in range(1,rows-1):
    for y in range(1,cols-1):
        if grid[x][y] != 'A':
            continue
        if check_x_mas(grid, x, y):
            count += 1

# Afficher le résultat
print(count)
