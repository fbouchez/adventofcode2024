#!/usr/bin/env python3
import sys
from AoC import *


# Lire la grille depuis un fichier
# with open("input-small.txt", "r") as file:
with open("input-mine.txt", "r") as file:
    grid = [line.strip() for line in file.readlines()]

# Le mot à chercher
word = "XMAS"

# Directions pour les déplacements (dx, dy) : 8 directions
directions = [
    (0, 1),   # Horizontal droite
    (0, -1),  # Horizontal gauche
    (1, 0),   # Vertical bas
    (-1, 0),  # Vertical haut
    (1, 1),   # Diagonale bas-droite
    (-1, -1), # Diagonale haut-gauche
    (1, -1),  # Diagonale bas-gauche
    (-1, 1),  # Diagonale haut-droite
]

# Fonction pour vérifier si le mot commence à une position donnée dans une direction donnée
def check_word(grid, word, x, y, dx, dy):
    rows, cols = len(grid), len(grid[0])
    for i in range(1,len(word)):
        nx, ny = x + i*dx, y + i*dy
        if nx < 0 or ny < 0 or nx >= rows or ny >= cols:
            return False

        if grid[nx][ny] != word[i]:
            return False
    return True

# Compter les occurrences du mot
count = 0
rows, cols = len(grid), len(grid[0])

for x in range(rows):
    for y in range(cols):
        if grid[x][y] != 'X':
            continue
        for dx, dy in directions:
            if check_word(grid, word, x, y, dx, dy):
                count += 1

# Afficher le résultat
print(count)
