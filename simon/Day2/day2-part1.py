import re

res = 0
colourLimits = {"red": 12, "green": 13, "blue": 14}

f = open("input.txt")
for line in f:
    # isolate colour names and counts, turn into list of tuples
    splits = re.split(r', |; |: | |\n', line)
    grabs = [(splits[i + 1], splits[i]) for i in range(2, len(splits) - 1, 2)]

    # checks whether the amount grabbed for any colour exceeds max possible, flagging as impossible if it does
    possible = True
    for colour in grabs:
        if int(colour[1]) > colourLimits[colour[0]]:
            possible = False
            break
    if possible:
        res += int(splits[1])

print(res)