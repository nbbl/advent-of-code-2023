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

# until we hit the starting character again, keep finding coordinates for the next move and get the letter they point
# at, incrementing the movement counter after each move
while curCharacter != "S":
    newCoords, newMove = makeNextMove(newCoords, curCharacter, newMove)
    curCharacter = grid[newCoords[0]][newCoords[1]]
    movementCounter += 1

# print the further point from the start character
print(round(movementCounter/2))