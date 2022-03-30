from collections import defaultdict
import math

def sudoku(grid):
    """
       solves or generates all possible
       square sudoku problems specified by a grid
       input: grid as  alist of lists with 0 marking free spots
       yield: one or more filled out grids as answers
    """
    n = len(grid)
    assert n == len(grid[0])
    m = int(math.sqrt(n))
    assert m*m==n # assumed a square grid

    dif_graph = to_dif_graph(m)

    def gget(x):
        i, j = x
        return grid[i][j]

    def gput(x, v):
        i, j = x
        grid[i][j] = v

    def not_taken_by(vs):
        for v in range(1, n + 1):
            if v not in vs:
                yield v

    def solve(i):
        if i >= len(dif_graph):
            yield grid
            return

        x, xs = dif_graph[i]

        if gget(x) != 0:
            yield from solve(i + 1)
        else:
            vs = {gget(y) for y in xs}
            for v in not_taken_by(vs):
                gput(x, v)
                yield from solve(i + 1)
                gput(x, 0)

    yield from solve(0)

def to_dif_graph(m):
    n = m * m
    d = defaultdict(set)
    for v in index_pairs(n):
        i, j = v
        d[v].update([(i, x) for x in range(n) if x != j])
    for v in index_pairs(n):
        j, i = v
        d[v].update([(x, i) for x in range(n) if x != j])
    for bs in blocs(m):
        for b in bs:
            d[b].update([c for c in bs if c != b])
    return list(d.items())


def index_pairs(n):
    for i in range(n):
        for j in range(n):
            yield i, j


def bloc_ranges(m):
    for k in range(m):
        yield k * m, (k + 1) * m


def bloc(u, v):
    for i in range(*u):
        for j in range(*v):
            yield i, j


def blocs(m):
    for u in bloc_ranges(m):
        for v in bloc_ranges(m):
            yield list(bloc(u, v))




# examples

def show(grid):
    for line in grid: print(line)
    print('')


def empty_grid(n):
    return [[0 for _ in range(n)] for _ in range(n)]


grid9 = [[2, 5, 0, 0, 3, 0, 9, 0, 1],
         [0, 1, 0, 0, 0, 4, 0, 0, 0],
         [4, 0, 7, 0, 0, 0, 2, 0, 8],
         [0, 0, 5, 2, 0, 0, 0, 0, 0],
         [0, 0, 0, 0, 9, 8, 1, 0, 0],
         [0, 4, 0, 0, 0, 3, 0, 0, 0],
         [0, 0, 0, 3, 6, 0, 0, 7, 2],
         [0, 7, 0, 0, 0, 0, 0, 0, 3],
         [9, 0, 3, 0, 0, 0, 6, 0, 4]]

escargot = [[1, 0, 0, 0, 0, 7, 0, 9, 0],
            [0, 3, 0, 0, 2, 0, 0, 0, 8],
            [0, 0, 9, 6, 0, 0, 5, 0, 0],
            [0, 0, 5, 3, 0, 0, 9, 0, 0],
            [0, 1, 0, 0, 8, 0, 0, 0, 2],
            [6, 0, 0, 0, 0, 4, 0, 0, 0],
            [3, 0, 0, 0, 0, 0, 0, 1, 0],
            [0, 4, 0, 0, 0, 0, 0, 0, 7],
            [0, 0, 7, 0, 0, 0, 3, 0, 0]]

""" correct escargot answer:
1 6 2 8 5 7 4 9 3 
5 3 4 1 2 9 6 7 8 
7 8 9 6 4 3 5 2 1 
4 7 5 3 1 2 9 8 6 
9 1 3 5 8 6 7 4 2 
6 2 8 7 9 4 1 3 5 
3 5 6 4 7 8 2 1 9 
2 4 1 9 3 5 8 6 7 
8 9 7 2 6 1 3 5 4 
"""

grid9a = empty_grid(9)

grid4 = [
    [0, 2, 3, 4],
    [3, 4, 0, 2],
    [4, 1, 2, 3],
    [2, 3, 4, 1]
]
""" answer
    [1, 2, 3, 4],
    [3, 4, 1, 2],
    [4,,1, 2, 3],
    [2,,3,,4,,1]
"""
grid4a = empty_grid(4)

grid16 = empty_grid(16)


def pd(d):
    for x, v in d.items():
        print(x, ':', len(v), '-->', v)


def pp(xs):
    for x in xs:
        print(x)


def answers(grid, just_count=False):
    print('PROBLEM:')
    show(grid)
    ctr = 0
    if just_count: print('just counting answers')
    for solved in sudoku(grid):
        ctr += 1
        if not just_count:
            print('SOLVED:')
            show(grid)
    print('answer count:', ctr)
    print('')


if __name__ == "__main__":
    # pd(to_dif_graph(2))
    # print(len(list(to_dif_graph(4))))
    # pp(bloc_ranges(3))

    answers(grid9)
    answers(escargot)
    answers(empty_grid(4), just_count=True)
    #answers(empty_grid(9))
    #answers(empty_grid(16))
    #answers(empty_grid(25))
