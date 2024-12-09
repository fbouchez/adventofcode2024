#!/usr/bin/env python3

import re

# Lire l'entrée depuis un fichier
with open("input-small2.txt", "r") as file:
# with open("input-mine.txt", "r") as file:
    corrupted_memory = file.read()

# Expressions régulières pour les instructions valides
mul_pattern = r"mul\((\d{1,3}),(\d{1,3})\)"

## version chatgpt qui fonctionne par chance (!)
# control_pattern = r"\b(do\(\)|don't\(\))"
#
control_pattern = r"do\(\)|don't\(\)"

# Trouver toutes les correspondances (mul et contrôle) dans l'ordre séquentiel
matches = re.findall(f"{mul_pattern}|{control_pattern}", corrupted_memory)
print("tous les matches:", matches)

# Trouver toutes les correspondances séquentiellement
# mul_matches = re.finditer(mul_pattern, corrupted_memory)
# control_matches = re.finditer(control_pattern, corrupted_memory)

# Mélanger toutes les correspondances en gardant l'ordre
# all_matches = sorted(
    # [(m.start(), m.group()) for m in mul_matches] +
    # [(m.start(), m.group()) for m in control_matches],
    # key=lambda x: x[0]
# )

# print("tous les matches:", all_matches)

# Initialiser l'état (mul est activé par défaut)
mul_enabled = True
total_sum = 0

# Traiter chaque correspondance
for match in matches:
    print("match suivant:", match)
    if match[2] == "do()":
        mul_enabled = True  # Activer les instructions mul
    elif match[2] == "don't()":
        mul_enabled = False  # Désactiver les instructions mul
    elif match[0] and match[1]:  # Une instruction mul(X,Y) valide
        if mul_enabled:
            x, y = int(match[0]), int(match[1])
            total_sum += x * y

# Afficher le résultat
print(total_sum)
