# Beurk code dégueu de chatgpt,
# il recalcule ce qu'il avait déjà en entrée
# => recherche d'intervalles alors que c'était le format
# d'entrée (lol)
#
def parse_disk_map(disk_map):
    """Parses the disk map and generates the initial block representation."""
    blocks = []
    file_id = 0
    for i in range(0, len(disk_map), 2):
        file_size = int(disk_map[i])
        free_space_size = int(disk_map[i + 1])

        # Add file blocks
        blocks.extend([file_id] * file_size)
        file_id += 1

        # Add free space blocks
        blocks.extend(["."] * free_space_size)
    
    return blocks

def compact_disk(blocks):
    """Compacts the blocks by moving whole files to the leftmost span of free space."""
    file_positions = {}

    # Find start and size of each file
    for i, block in enumerate(blocks):
        if block != ".":
            if block not in file_positions:
                file_positions[block] = [i, 0]  # Start index, size
            file_positions[block][1] += 1

    # Sort files by decreasing file ID
    for file_id in sorted(file_positions.keys(), reverse=True):
        start, size = file_positions[file_id]

        # Find the leftmost span of free space that can fit the file
        for i in range(len(blocks) - size + 1):
            if all(block == "." for block in blocks[i:i + size]):
                # Move the file
                blocks[i:i + size] = [file_id] * size
                blocks[start:start + size] = ["."] * size
                break

    return blocks

def calculate_checksum(blocks):
    """Calculates the checksum of the compacted blocks."""
    checksum = 0
    for position, block in enumerate(blocks):
        if block != ".":
            checksum += position * block
    return checksum

def main(disk_map):
    # Parse the disk map
    blocks = parse_disk_map(disk_map)

    # Compact the disk
    compacted_blocks = compact_disk(blocks)

    # Calculate the checksum
    checksum = calculate_checksum(compacted_blocks)
    return checksum

# Example usage
disk_map = "2333133121414131402"  # Replace this with the puzzle input
result = main(disk_map)
print("Checksum:", result)
