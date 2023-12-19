def findValidMove(gridArray, startPosition):
    """
    Takes a grid array and a Starting Position expressed as coordinates, and checks the surrounding cells for a valid
    direction to move in. Returns the coordinates and direction of the first valid move found.

    :param gridArray (*list): Grid array of all positions
    :param startPosition (list): Coordinates of the starting position, expressed as a list of two indices referring to
    the x and y axes within the GridArray
    :return (list): coordinates of a valid movement, expressed as a list of two indices
    :return (string): string indicating the direction moved from the start position to reach the valid coordinates
    """
    upCoords = [startPosition[0]-1, startPosition[1]]
    downCoords = [startPosition[0]+1, startPosition[1]]
    leftCoords = [startPosition[0], startPosition[1]-1]
    rightCoords = [startPosition[0], startPosition[1]+1]

    if gridArray[upCoords[0]][upCoords[1]] in ["F", "7" "|"]:
        return upCoords, "up"
    elif gridArray[downCoords[0]][downCoords[1]] in ["J", "L", "|"]:
        return downCoords, "down"
    elif gridArray[leftCoords[0]][leftCoords[1]] in ["F", "L", "-"]:
        return leftCoords, "left"
    elif gridArray[rightCoords[0]][rightCoords[1]] in ["J", "7", "-"]:
        return rightCoords, "right"

def makeNextMove(currentPosition, currentCharacter, previousMove):
    """
    Takes the current position in a grid expressed as coordinates, the character at that position in the grid, and the
    direction of a previous move (so as not to move backwards), and returns the coordinates of the only next valid move

    :param currentPosition (list): Coordinates of the starting position, expressed as a list of two indices referring to
    the x and y axes of a grid
    :param currentCharacter (string): The character held in the grid at the coordinates stored in currentPosition
    :param previousMove (string): A word indicating the direction of the previous movement e.g. "up", "left", "down", or
    "right"
    :return (list): coordinates of the only valid movement, expressed as a list of two indices
    :return (string):  string indicating the direction moved from the current position to reach the valid coordinates
    """
    upCoords = [currentPosition[0]-1, currentPosition[1]]
    downCoords = [currentPosition[0]+1, currentPosition[1]]
    leftCoords = [currentPosition[0], currentPosition[1]-1]
    rightCoords = [currentPosition[0], currentPosition[1]+1]
    if currentCharacter == "J":
        if previousMove == "right":
            return upCoords, "up"
        else:
            return leftCoords, "left"
    elif currentCharacter == "L":
        if previousMove == "left":
            return upCoords, "up"
        else:
            return rightCoords, "right"
    elif currentCharacter == "F":
        if previousMove == "left":
            return downCoords, "down"
        else:
            return rightCoords, "right"
    elif currentCharacter == "7":
        if previousMove == "right":
            return downCoords, "down"
        else:
            return leftCoords, "left"
    elif currentCharacter == "|":
        if previousMove == "down":
            return downCoords, "down"
        else:
            return upCoords, "up"
    elif currentCharacter == "-":
        if previousMove == "right":
            return rightCoords, "right"
        else:
            return leftCoords, "left"
    else:
        print("invalid character:", currentCharacter)


# create a grid array of the characters in the input
f = open("input.txt")
grid = [list(line.rstrip()) for line in f]

# find the coordinates of the Starting character ("S")
startCoords = [[y, x] for y, line in enumerate(grid) for x, char in enumerate(line) if char == "S"][0]

# perform the first movement by finding a valid move, storing the coordinates and target character of that movement
newCoords, newMove = findValidMove(grid, startCoords)
curCharacter = grid[newCoords[0]][newCoords[1]]
movementCounter = 1

# create a dictionary to store the coordinates of each coordinates along the route, with the key being the x indice.
# This will provide a list of all the line segments in each column of the grid
lineCoords = {}
lineCoords[newCoords[1]] = [newCoords[0]]

# until we hit the starting character again, keep finding coordinates for the next move and get the letter they point
# at, incrementing the movement counter after each move
while curCharacter != "S":
    newCoords, newMove = makeNextMove(newCoords, curCharacter, newMove)
    curCharacter = grid[newCoords[0]][newCoords[1]]
    if newCoords[1] in lineCoords:
        lineCoords[newCoords[1]].append(newCoords[0])
    else:
        lineCoords[newCoords[1]] = [newCoords[0]]
    movementCounter += 1

lineCoords = {k: sorted(v) for k, v in lineCoords.items()}

# count of the points inside the loop
interiorPoints = 0

# for each column, count how many times the column is 'crossed over' by the line of the circuit. If there has been an
# odd number of crossings, we are inside the circuit, so we count any cells which are not part of the line. If there has
# been an even number of crossings, we are outside the loop and do not add to the count
for x in range(len(grid[0])):
    crossings = 0
    previousCorner = ""
    inside = False
    for y in range(len(grid)):
        curChar = grid[y][x]
        if x in lineCoords and y in lineCoords[x]:
            if curChar == "-":
                crossings += 1
            elif curChar in ["F", "J", "7", "L"]:
                if curChar == "L" and previousCorner == "F":
                    crossings += 2
                elif curChar == "J" and previousCorner == "7":
                    crossings += 2
                elif curChar == "J" and previousCorner == "F":
                    crossings += 1
                elif curChar == "L" and previousCorner == "7":
                    crossings += 1
                previousCorner = curChar
            elif curChar == "S":
                previousCorner = "F"
        elif inside:
            interiorPoints += 1
        # check if we're now inside the loop i.e. must not have had an even number of crossings or 0 crossings
        inside = (crossings > 0 and crossings % 2 != 0)

print(interiorPoints)
