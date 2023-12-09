f = open("input.txt")

res = 0

# list containing each history as a list of ints
histories = ([[int(x) for x in line.split()] for line in f])

# for each history, find the differences between each item and append them as a new 'level' to a list of levels. If all
# the numbers in the new level are zeros, stop, otherwise repeat the process for the numbers in this new level
for history in histories:
    historyLevels = []
    historyLevels.append(history)
    diffs = [1] # needs to contain a non-zero item to begin with
    while not all(item == 0 for item in diffs):
        diffs = []
        for i in range(1, len(history)):
            diffs.append(history[i] - history[i-1])
        historyLevels.append(diffs)
        history = diffs

    # find the next number in the original sequence by summing the final number from each depth
    sum = 0
    for level in historyLevels:
        sum += level[-1]
    res += sum

print(res)

