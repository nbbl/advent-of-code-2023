import re
res = 0

f = open("input.txt")

def increment_score(score):
    if score == 0:
        return 1
    return score*2

for line in f:
    score = 0
    splitLine = re.split(': | \| ', line.rstrip())
    winNums, ourNums = re.findall(r'\d+', splitLine[1]), re.findall(r'\d+', splitLine[2])
    for n in ourNums:
        if n in winNums:
            score = increment_score(score)
    res += score

print(res)