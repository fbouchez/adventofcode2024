#!/usr/bin/env python3
# import sys
# from AoC import *

def is_safe(report):
    # Calculer les différences entre niveaux adjacents
    differences = [report[i+1] - report[i] for i in range(len(report) - 1)]
    
    # Vérifier la monotonie stricte (tout croissant ou tout décroissant)
    all_increasing = all(1 <= diff <= 3 for diff in differences)
    all_decreasing = all(-3 <= diff <= -1 for diff in differences)
    
    # Le rapport est sûr si toutes les différences sont dans [1, 3] ou [-3, -1]
    return all_increasing or all_decreasing

# Lire les rapports depuis un fichier
with open("input-mine.txt") as file:
    reports = [[int(level) for level in line.split()] for line in file]

# Compter les rapports sûrs
safe_reports = sum(is_safe(report) for report in reports)


print("Nombre de rapports sûrs :", safe_reports)
