import re

def map_number(input, cypher):
    """
    Takes a number and a dictionary containing information about a mapping cypher and attempts to map the number using
    the cypher, returning either the mapped number or False

    :param input (int): the number to be mapped
    :param cypher (dict): a dictionary containing details of a cypher for mapping
    :return: The mapped result (int) or False (bool)
    """
    if cypher['source'] <= input <= (cypher['source'] + cypher['rangeLength']):
        return cypher['destination'] + (input - cypher['source'])
    else:
        return False

f = open("input.txt")

# get seed numbers from first line
seeds = [int(x) for x in re.findall(r'\d+', f.readline())]

# get map names and associated number ranges
mapsSplit = re.findall(r'([a-z-]+) map:|(\d+\s\d+\s\d+)+', f.read().replace("\n", " "))

# create a dictionary where Key = 'Map Name' and Values = a list of dictionaries, one dictionary per line, containing
# the Destination, Source, and RangeLength number values for each line.
# Simultaneously creates an ordered list of the maps since we need to know the order to do the data translation
mapsDict = {}
mapsOrdered = []
mapName = ""
for match in mapsSplit:
    if match[0]:
        mapName = match[0]
        mapsDict[mapName] = []
        mapsOrdered.append(mapName)
    else:
        destination, source, rangeLength = match[1].split()
        mapsDict[mapName].append({'destination': int(destination), 'source': int(source), 'rangeLength': int(rangeLength)})

# create list to store the final location of each seed
locations = []

# for each seed number, send the number through each mapping stage, either transforming it to a new number through the
# use of a valid mapping cyher, or retaining the number into the next mapping stage if no valid mapping cypher is found.
for seed in seeds:
    currentNumber = seed
    for map in mapsOrdered:
        print(map)
        for cypher in mapsDict[map]:
            mappedNumber = map_number(currentNumber, cypher)
            if mappedNumber:
                currentNumber = mappedNumber
                break
    # once the seed has passed through all mapping stages, record the final mapped location
    locations.append(currentNumber)

# show the lowest ("closest") location
print(min(locations))