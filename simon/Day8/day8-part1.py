import re

f = open("input.txt")

# convert L and R directions into 0 and 1 to be using for indexing into the mapping values later
LRNums = list([int(x) for x in f.readline().rstrip().replace("L", "0").replace("R", "1")])
f.readline()

# create a dictionary of the node keys and a tuple containing the Left and Right value for each node
nodeDict = {}
for line in f:
    maps = re.findall(r'\w+', line.strip())
    nodeDict[maps[0]] = (maps[1], maps[2])

# starting at AAA, follow the LR codex through the nodes until we find ZZZ, counting the steps as we go
res = "AAA"
stepCount = 0
while res != "ZZZ":
    for LR in LRNums:
        stepCount += 1
        res = nodeDict[res][LR]

print(stepCount)