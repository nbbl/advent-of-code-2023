def expandSpace(grid):
    '''
    Takes a grid and adds a new row of space for each row of space already in the grid

    :param grid (list*): A grid array where space must be expanded
    :return (list*): The expanded grid
    '''
    newGrid = []
    for line in grid:
        newGrid.append(line)
        if all(x == '.' for x in line):
            newGrid.append(['.' for c in line])
    return newGrid

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

# Create grid from input
f = open("input.txt")
grid = [list(line.rstrip()) for line in f]

# Expands space vertically and then horizontally
grid = expandSpace(grid)
grid = expandSpace(rotateGrid(grid, 1))

# Get coordinates for each galaxy
galaxyCoords = []
for y in range(len(grid)):
    for x in range(len(grid[y])):
        if grid[y][x] == '#':
            galaxyCoords.append([x, y])

# Finds the distance between each galaxy by finding the absolute difference between each of the x coordinates and each
# of the y coordinates, summing the distances
res = 0
for i in range(len(galaxyCoords)):
    for j in range(i + 1, len(galaxyCoords)):
        diff = (abs(galaxyCoords[i][0]-galaxyCoords[j][0]) + abs(galaxyCoords[i][1]-galaxyCoords[j][1]))
        res += diff

print(res)