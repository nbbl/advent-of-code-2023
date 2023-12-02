import re
import numpy

res = 0

f = open("input.txt")
for line in f:
    # isolate colour names and counts, turn into list of tuples
    splits = re.split(r', |; |: | |\n', line)
    grabs = [(splits[i + 1], splits[i]) for i in range(2, len(splits) - 1, 2)]

    # create dict of highest number grabbed for each colour
    colourMax = {"red": 0, "blue": 0, "green": 0}
    for colour in grabs:
        if int(colour[1]) > colourMax[colour[0]]:
            colourMax[colour[0]] = int(colour[1])

    # add product of maximums to result
    res += numpy.product(list(colourMax.values()))

print(res)