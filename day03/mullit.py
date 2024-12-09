#!/usr/bin/env python3
import sys
from AoC import *

import re

# Entrée de mémoire corrompue
# corrupted_memory = (
    # "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
# )
# Lire l'entrée depuis un fichier
# with open("input-small.txt", "r") as file:
with open("input-mine.txt", "r") as file:
    corrupted_memory = file.read()

# Expression régulière pour correspondre aux instructions valides "mul(X,Y)"
pattern = r"mul\((\d{1,3}),(\d{1,3})\)"

# Trouver toutes les correspondances
matches = re.findall(pattern, corrupted_memory)

# Calculer la somme des produits
total_sum = sum(int(x) * int(y) for x, y in matches)

# Afficher le résultat
print(total_sum)
