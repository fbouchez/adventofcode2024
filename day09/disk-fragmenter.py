#!/usr/bin/env python3
import sys

def parse_disk_map(disk_map):
    """Parses the disk map and generates the initial block representation."""
    blocks = []
    file_id = 0
    for i in range(0, len(disk_map), 2):
        file_size = int(disk_map[i])

        # ici erreur aussi de chatgpt car le dernier 
        # est un file et non un espace libre
        free_space_size = 0
        if i+1 < len(disk_map):
            free_space_size = int(disk_map[i + 1])

        # Add file blocks
        blocks.extend([file_id] * file_size)
        file_id += 1

        # Add free space blocks
        blocks.extend(["."] * free_space_size)

    return blocks


# argh, chatgpt va consommer toutes nos ressources computationnelle en 
# faisant des algos en n² au lieu de n !
def compact_disk_origchatgpt(blocks):
    """Compacts the blocks by moving file blocks to the leftmost free space."""
    # Compacting happens by shifting blocks to the left
    write_pos = 0  # The next free position to write a file block

    for read_pos in range(len(blocks)):
        if blocks[read_pos] != ".":  # If it's a file block
            blocks[write_pos] = blocks[read_pos]
            if write_pos != read_pos:
                blocks[read_pos] = "."  # Leave free space behind
            write_pos += 1
        # print(f"{''.join(map(str,blocks))}")

    return blocks

def compact_disk_origchatgpt2(blocks):
    """Compacts the blocks by moving file blocks to the leftmost free space, prioritizing blocks further away."""
    for i in range(len(blocks) - 1, -1, -1):
        if blocks[i] != ".":
            # Find the first free space to the left
            for j in range(len(blocks)):
                if blocks[j] == ".":
                    blocks[j] = blocks[i]  # Move the file block
                    blocks[i] = "."  # Free the original position
                    break
        print(f"{''.join(map(str,blocks))}")
    return blocks

def compact_disk(blocks):
    """Compacts the blocks by moving file blocks to the leftmost free space, prioritizing blocks further away."""
    j = 0
    for i in range(len(blocks) - 1, -1, -1):
        if blocks[i] != ".":
            # Find the first free space to the left
            while blocks[j] != '.':
                j += 1


            # chatgpt avait oublié de s'arrêter ici !
            if j > i:
                break

            blocks[j] = blocks[i]  # Move the file block
            blocks[i] = "."  # Free the original position

        # print(f"{''.join(map(str,blocks))}")
    return blocks


def calculate_checksum(blocks):
    """Calculates the checksum of the compacted blocks."""
    checksum = 0
    for position, block in enumerate(blocks):
        if block != ".":
            current = position * block
            # print(f"check for {block} at {position} : {current}")
            checksum += position * block
    return checksum

def main(disk_map):
    # Parse the disk map
    blocks = parse_disk_map(disk_map)

    # print(f"{blocks}")

    # Compact the disk
    compacted_blocks = compact_disk(blocks)

    # Calculate the checksum
    checksum = calculate_checksum(compacted_blocks)
    return checksum

# Example usage
# disk_map = "2333133121414131402"  # Replace this with the puzzle input
disk_map = input().strip()
result = main(disk_map)
print("Checksum:", result)
