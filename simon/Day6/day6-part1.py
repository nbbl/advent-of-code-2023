import re

f = open("input.txt")
# print(f.read())
times = [int(x) for x in re.findall(r'(\d+)', f.readline())]
distances = [int(x) for x in re.findall(r'\d+', f.readline())]

res = 1

for raceLength, targetDistance in list(zip(times, distances)):
    waysToWin = 0
    for i in range(raceLength):
        buttonPress = i
        remainingSeconds = raceLength-i
        distanceTravelled = buttonPress * remainingSeconds
        if distanceTravelled > targetDistance:
            waysToWin += 1

    res *= waysToWin

print(res)