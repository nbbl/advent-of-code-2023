import copy

def rotateGrid(grid, rotations):
    '''
    Takes a grid array and rotates it 90 degrees clockwise a specified number of times

    :param grid (list*): A grid array to be rotated
    :param rotations (int): The number of times to rotate the grid 90 degrees clockwise
    :return (list*): The rotated grid
    '''
    for i in range(rotations):
        grid = [list(reversed(x)) for x in zip(*grid)]
    return grid

def expandSpace(grid, coords):
    '''
    Takes a grid and coordinates for galaxies within that grid, and returns an updated set of coordinates reflecting
    where rows and columns of pure space in the grid have been expanded by a factor of 1000000

    :param grid (list*): A grid array to be expanded
    :param coords (list*): A list of coordinates of galaxies within the grid
    :return (list*): A list of coordinates updated to reflect expanded space in the grid
    '''
    newCoords = copy.deepcopy(coords)
    for i in range(len(grid)):
        if all(x == '.' for x in grid[i]):
            for count in range(len(coords)):
                if coords[count][1] > i:
                    newCoords[count][1] = newCoords[count][1] + 999999

    grid = rotateGrid(grid, 1)
    for i in range(len(grid)):
        if all(x == '.' for x in grid[i]):
            for count in range(len(coords)):
                if coords[count][0] > i:
                    newCoords[count][0] = newCoords[count][0] + 999999

    return newCoords

# Create grid from input
f = open("input.txt")
grid = [list(line.rstrip()) for line in f]

# Get coordinates for each galaxy
galaxyCoords = []
for y in range(len(grid)):
    for x in range(len(grid[y])):
        if grid[y][x] == '#':
            galaxyCoords.append([x, y])

# Update galaxy coordinates to account for expanded space
galaxyCoords = expandSpace(grid, galaxyCoords)

# Finds the distance between each galaxy by finding the absolute difference between each of the x coordinates and each
# of the y coordinates, summing the distances
res = 0
for i in range(len(galaxyCoords)):
    for j in range(i + 1, len(galaxyCoords)):
        diff = (abs(galaxyCoords[i][0]-galaxyCoords[j][0]) + abs(galaxyCoords[i][1]-galaxyCoords[j][1]))
        res += diff

print(res)