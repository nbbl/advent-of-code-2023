import re
from math import gcd
from functools import reduce

def lcm(denominators):
    """
    Find the LCM of a list of integers. Stolen from Stack Overflow.

    :param denominators (*int): a list of integers to find the LCM of
    :return (int): the LCM of the input
    """
    return reduce(lambda a,b: a*b // gcd(a,b), denominators)

f = open("input.txt")

# convert L and R directions into 0 and 1 to be using for indexing into the mapping values later
LRNums = list([int(x) for x in f.readline().rstrip().replace("L", "0").replace("R", "1")])
f.readline()

# create a list of keys whose last letter is A
ghostStartNodes = []

# create a dictionary of the node keys and a tuple containing the Left and Right value for each node
nodeDict = {}
for line in f:
    maps = re.findall(r'\w+', line.strip())
    # if last character is 'A', add to list of ghost starter nodes
    if maps[0][2] == "A":
        ghostStartNodes.append(maps[0])
    nodeDict[maps[0]] = (maps[1], maps[2])

# for each ghostStartNode, find the stepcount to a node ending in Z and add it to the stepCounts list
stepCounts = []
for ghost in ghostStartNodes:
    res = ghost
    stepCount = 0
    while res[-1] != "Z":
        for LR in LRNums:
            stepCount += 1
            res = nodeDict[res][LR]
    stepCounts.append(stepCount)

# find LCM of all stepCounts
print(lcm(stepCounts))

