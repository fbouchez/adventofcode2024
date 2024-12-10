#!/usr/bin/env python3
import sys
from collections import defaultdict
from AoC import *


part2 = True

def parse_map():
    """Parse the input map into a grid and identify antenna positions."""
    grid = []
    antennas = defaultdict(list)
    for y, row in enumerate(sys.stdin):
        grid.append(row.strip())
        for x, char in enumerate(row):
            if char != ".":
                antennas[char].append((x, y))
    return grid,antennas, x+1, y+1


def calculate_antinodes_freq(positions,width,height, antinodes):
    n = len(positions)
    for i in range(n):
        for j in range(i + 1, n):
            x1, y1 = positions[i]
            x2, y2 = positions[j]
            # Calculate the difference between the antennas
            # vecteur de 1 vers 2
            dx, dy = x2 - x1, y2 - y1

            # ici on part de 1 et vecteur en sens inverse
            # ici on continue avec le même vecteur

            mult = 0

            flag = True
            while flag:
                ax1, ay1 = x1 - mult*dx, y1 - mult*dy
                ax2, ay2 = x2 + mult*dx, y2 + mult*dy
                mult += 1

                flag = False
                flag |= add_antinodes(ax1, ay1, width, height, antinodes)
                flag |= add_antinodes(ax2, ay2, width, height, antinodes)


def add_antinodes(xx, yy, width, height, antinodes):
    # Check bounds and add valid antinodes
    if 0 <= xx < width and 0 <= yy < height:
        antinodes.add((xx, yy))
        return True
    return False




def calculate_antinodes(antennas, width, height):
    """Calculate antinode positions for antennas of the same frequency."""
    antinodes = set()
    for frequency, positions in antennas.items():
        calculate_antinodes_freq(positions,width,height, antinodes)
    return antinodes

def count_unique_antinodes():
    """Count unique locations containing antinodes."""
    grid, antennas, width, height = parse_map()
    antinodes = calculate_antinodes(antennas, width, height)

    display_map_with_antinodes(grid, antinodes)
    return len(antinodes)


def display_map_with_antinodes(input_map, antinodes):
    """Display the map with antinodes marked by '#'."""
    map_with_antinodes = [list(row) for row in input_map]  # Convert to mutable list of lists

    for x, y in antinodes:
        if map_with_antinodes[y][x] == ".":  # Only replace empty spaces
            map_with_antinodes[y][x] = "#"

    # Convert back to strings and print
    for row in map_with_antinodes:
        print("".join(row))


result = count_unique_antinodes()
print("Unique locations with antinodes:", result)
