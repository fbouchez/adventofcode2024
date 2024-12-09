#!/usr/bin/env python3
import sys
from AoC import *

# Read the input file
# with open("input-small.txt", "r") as file:
with open("input-mine.txt", "r") as file:
    data = file.read().strip().split("\n\n")

# Parse the ordering rules
rules = []
for line in data[0].splitlines():
    x, y = map(int, line.split("|"))
    rules.append((x, y))

# Parse the updates
updates = [list(map(int, update.split(","))) for update in data[1].splitlines()]

# Function to check if an update is valid
def is_valid_update(update, rules):
    index_map = {page: i for i, page in enumerate(update)}
    for x, y in rules:
        if x in update and y in update:
            if index_map[x] > index_map[y]:
                return False
    return True

# Find valid updates and their middle pages
valid_middle_pages = []
for update in updates:
    if is_valid_update(update, rules):
        print("Valide:", update)
        assert len(update) % 2 == 1
        middle_index = len(update) // 2
        # valid_middle_pages.append(sorted(update)[middle_index]) ## initial chatgpt
        valid_middle_pages.append(update[middle_index])

# Sum the middle pages of valid updates
result = sum(valid_middle_pages)
print(result)
