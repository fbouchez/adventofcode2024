def read_lists_from_file(filename):
    try:
        with open(filename, 'r') as file:
            list1, list2 = [], []
            for line in file:
                # Extraire les valeurs des deux colonnes
                values = line.strip().split()
                if len(values) != 2:
                    raise ValueError("Chaque ligne doit contenir exactement deux valeurs séparées par un espace.")
                # Ajouter aux listes respectives
                list1.append(int(values[0].strip()))
                list2.append(int(values[1].strip()))
            return list1, list2
    except FileNotFoundError:
        print(f"Erreur : Le fichier '{filename}' est introuvable.")
    except ValueError as e:
        print(f"Erreur : {e}")
    return None, None


# Exemple d'utilisation
filename = 'input-mine.txt'
list1, list2 = read_lists_from_file(filename)


def calculate_total_distance(left_list, right_list):
    # Trier les deux listes
    left_list.sort()
    right_list.sort()
    
    # Calculer la distance totale
    total_distance = sum(abs(l - r) for l, r in zip(left_list, right_list))
    return total_distance

# Exemple donné dans l'énoncé
left_list = [3, 4, 2, 1, 3, 3]
right_list = [4, 3, 5, 3, 9, 3]

# Calculer la distance totale
result = calculate_total_distance(left_list, right_list)
result2 = calculate_total_distance(list1, list2)
print(f"Distance totale : {result}")
print(f"Distance totale : {result2}")


from collections import Counter

# Charger les deux listes
with open("input-mine.txt") as file:
    left_list = []
    right_list = []
    for line in file:
        left, right = map(int, line.split())
        left_list.append(left)
        right_list.append(right)

# Compter les occurrences dans la liste droite
right_count = Counter(right_list)


# On peut le faire à la main ainsi:
rc = {}
for e in right_list:
    if e in rc:
        rc[e] += 1
    else:
        rc[e] = 1

right_count = rc



print("right_count:", right_count)

# Calculer le score de similarité
similarity_score = 0
for num in left_list:
    similarity_score += num * right_count.get(num, 0)

print("Le score de similarité est :", similarity_score)

