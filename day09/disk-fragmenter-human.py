

import heapq

# Part 2
#
# First wrong result: 6337609075493 (too high)
# Correct             6321896265143

def parse_disk_map(disk_map):
    """Parses the disk map and generates the initial block representation."""
    files = []
    index = 0

    frees = []

    for i in range(0, len(disk_map), 2):

        file_size = int(disk_map[i])

        files.append((index,file_size))

        index += file_size

        if i+1 >= len(disk_map):
            break

        free_space_size = int(disk_map[i + 1])

        if free_space_size == 0:
            continue

        # heapq.heappush(frees, (index, free_space_size))
        frees.append((index, free_space_size))
        index += free_space_size

    return files, frees


def compact_disk(files, frees):

    moved = False
    num_files = len(files)-1

    for fident_prime, (fidx, fsiz) in enumerate(reversed(files)):
        fident = num_files - fident_prime

        for i, (freeidx, freesize) in enumerate(frees):
            if freeidx >= fidx:
                break
            if freesize < fsiz:
                continue

            # we found some space
            rem = freesize - fsiz

            files[fident] = (freeidx, fsiz)

            frees.pop(i)
            if rem != 0:
                frees.append((freeidx + fsiz, rem))

            frees.sort()
            # heapq.heapify(frees)
            moved = True

            # print(f"moved {fident} of size {fsiz} from {fidx} to {freeidx}")
            # print(f"now free is {frees}")
            break

def calculate_checksum(files):
    """Calculates the checksum of the compacted blocks."""
    checksum = 0

    for fident, (fidx, fsiz) in enumerate(files):
        current = fident * (fsiz * (2 * fidx + fsiz - 1) // 2)
        # current = 0
        # for idx in range(fidx, fidx+fsiz):
            # current += fident*idx

        # print(f"current for {fident}: {current}")
        checksum += current
    return checksum




def main(disk_map):
    # Parse the disk map
    files,frees = parse_disk_map(disk_map)

    print(f"{files=} and {frees=}")

    # Compact the disk
    compact_disk(files, frees)

    # Calculate the checksum
    checksum = calculate_checksum(files)
    return checksum



disk_map = input().strip()
result = main(disk_map)
print("Checksum:", result)
