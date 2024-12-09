#!/usr/bin/env python3

# Lire les règles à partir d'un fichier
with open("input-small.txt", "r") as file:
# with open("input-mine.txt", "r") as file:
    rules_section = file.read().strip().split("\n\n")[0]

# Analyser les règles
rules = [line.split("|") for line in rules_section.splitlines()]

# Générer le fichier DOT
with open("dag.dot", "w") as dot_file:
    dot_file.write("digraph PageOrder {\n")
    dot_file.write("  rankdir=LR;\n")  # Orientation gauche-droite (facultatif)
    dot_file.write("  node [shape=circle, fontsize=12];\n")

    # Ajouter chaque règle comme une arête
    for x, y in rules:
        dot_file.write(f"  {x} -> {y};\n")

    dot_file.write("}\n")

print("DAG généré dans le fichier dag.dot.")
