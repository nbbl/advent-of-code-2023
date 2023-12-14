f = open("input.txt")

res = 0

# list containing each history as a list of ints
histories = ([[int(x) for x in line.split()] for line in f])

# for each history, find the differences between each item and append them as a new 'level' to a list of levels. If all
# the numbers in the new level are zeros, stop, otherwise repeat the process using the numbers in this new level
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

    # put the all zeros level first for ease
    historyLevels.reverse()

    # starting with the all zeros level, find the difference between the first element of the next level up and the
    # first element of the current level. Then find the difference between the first element of the next level and the
    # previously identified difference. The final difference will be the new starting value for the original history
    difference = 0
    for i in range(len(historyLevels)-1):
        difference = historyLevels[i + 1][0] - difference
    # add the final difference (new starting value for the original history) to the output result
    res += difference

print(res)

