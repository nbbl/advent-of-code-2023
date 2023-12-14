import sys
from sympy import Symbol
from sympy.polys.specialpolys import interpolating_poly

x = Symbol('x')

def solve(getIndex, file):
    with open(file) as f:
        lines = [list(map(int, line.strip().split())) for line in f.readlines()]
        return sum([interpolate(line, getIndex) for line in lines])

def interpolate(ys, getIndex):
    return interpolating_poly(len(ys), x, list(range(1, len(ys) + 1)), ys).as_poly().eval(getIndex(ys))

if __name__ == "__main__":
    if sys.argv[1] == "part1":
        getIndex = lambda l: len(l) + 1
    else:
        getIndex = lambda l: 0
    print(solve(getIndex, sys.argv[2]))
