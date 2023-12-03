def getNumberLength(row, index):
    """
    Returns the "length" of a number whose digits are split into an array by reading forward in the array until
    it locates a non-digit number e.g. in the array ["4", "2", "6", "9", "."], the  number is 4269, which has a
    length of 4.

    :param row [list]: the array which contains the number
    :param index [int]: the index at which the number begins
    :return numberLength [int]: the length of the number found in the array
    """
    foundEnd = False
    numberLength = 1
    while not foundEnd:
        index += 1
        if row[index].isnumeric():
            numberLength += 1
        else:
            foundEnd = True
    return numberLength

def getSearchWindow(index, numLength):
    """
    Produces a list of indices based on the starting index and length of an item, for the purpose of searching cells
    adjacent to the item in a grid array e.g. with a starting Index of 2 and a length of 3, this will return
    [1, 2, 3, 4, 5], which can be used to access the elements above, below, and either side of the item in a grid
    array, as per the following example:

    In the below diagram, '#' represents the number at index 2 with a length of 3. The returned window can be used to
    help access the elements marked with a 'X'

       0  1  2  3  4  5  6
    0 [ ][ ][ ][ ][ ][ ][ ]
    1 [ ][X][X][X][X][X][ ]
    2 [ ][X][#][#][#][X][ ]
    3 [ ][X][X][X][X][X][ ]
    4 [ ][ ][ ][ ][ ][ ][ ]

    :param index [int]: the starting index of the item
    :param numLength [int]: the length of the item
    :return window [list]: a list of indices
    """
    window = []
    for n in range(index-1, index+numLength+1):
        window.append(n)
    return window

def checkSymbols(rows, searchWindow):
    """
    Takes a grid array and a list of indices, and searches each row of the grid at the specified indices, checking
    whether any element is a symbol other than "."

    :param rows [list of lists]: a grid array in which to search for symbols at the specified indices of each row
    :param searchWindow [list]: a list of indices to search on each row of the grid array
    :return symbolFound [bool]: returns True if a symbol was located in the specified indices
    """
    symbolFound = False
    for index in searchWindow:
        for row in rows:
            if not (row[index].isnumeric() or row[index] == "."):
                symbolFound = True
                break
    return symbolFound

# initialises final calculation
res = 0

# initialises grid array for storing input, adding a buffer row to the top to prevent indexing errors later
grid = []
f = open("input.txt")
buffer = ["."] * (len(f.readline())+2)
grid.append(buffer)
f.seek(0)

# adds each row of input to grid array, with a buffer character at start and end of each row to prevent index errors
for line in f:
    newline = ["."]
    for character in line.rstrip():
        newline.append(character)
    newline.append(".")
    grid.append(newline)

# adds another buffer row to the end of the grid array to prevent indexing errors
grid.append(buffer)

# iterates over each row of the grid array looking for numbers - then when a number is found, identifies the
# indices of the adjacent cells and searches them for symbols. If a symbol is present, the number is added to the result
for row in range(len(grid)):
    for i in range(len(grid[row])):
        # the second condition ensures each number in the grid only triggers the symbol search once
        if grid[row][i].isnumeric() and not grid[row][i - 1].isnumeric():
            numLength = getNumberLength(grid[row], i)
            numInt = int("".join(grid[row][i:i + numLength]))
            searchWindow = getSearchWindow(i, numLength)
            symbolPresent = checkSymbols([grid[row - 1], grid[row], grid[row + 1]], searchWindow)
            if symbolPresent:
                res += numInt

print(res)
