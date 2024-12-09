import sys
import argparse

def debug(*a, **kw):
    if args.debug:
        print(*a, **kw, file=sys.stderr)

parser = argparse.ArgumentParser(description="AoC solve")

parser.add_argument(
    "--debug",
    "-d",
    action=argparse.BooleanOptionalAction,
    help="debug mode: verbose printing",
)

args = parser.parse_args()


def parseInts(l, sep=' '):
    return [int(x) for x in l.split(sep)]



def scan_char_map():
    chmap = []

    NCOLS = None

    for line in sys.stdin:
        line = line.strip()
        if not NCOLS:
            NCOLS = len(line)
        else:
            assert NCOLS == len(line)
        chmap.append(list(line))

    NROWS = len(chmap)

    return chmap, NROWS, NCOLS


def set_rows_cols(nrows, ncols):
    global NROWS, NCOLS
    NROWS = nrows
    NCOLS = ncols

def valid(r, c):
    return 0 <= r < NROWS and 0 <= c < NCOLS


def debugcharmap(cm):
    for row in cm:
        debug(''.join(row))



def next_in_dir(row, col, d):
    dr, dc = delta_coord[d]
    nrow = row + dr
    ncol = col + dc
    return nrow, ncol




conv = {
    '.': (),
    '-': ('W','E'),
    '|': ('N','S'),
    '7': ('W','S'),
    'J': ('N','W'),
    'F': ('E','S'),
    'L': ('N','E'),
    'S': ('N','S','E','W')
}

opposite = {
    'N': 'S',
    'S': 'N',
    'E': 'W',
    'W': 'E'
}

is_horiz = {
    'N': False,
    'S': False,
    'E': True,
    'W': True,
}


delta_coord = {
    'N': (-1, 0),
    'S': ( 1, 0),
    'E': (0,  1),
    'W': (0, -1),
}


directions = opposite.keys()


