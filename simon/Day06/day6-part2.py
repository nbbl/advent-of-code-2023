#     ____        __  _                           _
#    / __ \____ _/ /_(_)__  ____  ________       (_)____
#   / /_/ / __ `/ __/ / _ \/ __ \/ ___/ _ \     / / ___/
#  / ____/ /_/ / /_/ /  __/ / / / /__/  __/    / (__  )
# /_/    \__,_/\__/_/\___/_/ /_/\___/\___/    /_/____/
#   ____ _  _   __(_)____/ /___  _____
#  / __ `/ | | / / / ___/ __/ / / / _ \
# / /_/ /  | |/ / / /  / /_/ /_/ /  __/
# \__,_/   |___/_/_/   \__/\__,_/\___/

import re

f = open("input.txt")
times = [int(x) for x in re.findall(r'(\d+)', f.readline().replace(" ", ""))]
distances = [int(x) for x in re.findall(r'\d+', f.readline().replace(" ",""))]

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