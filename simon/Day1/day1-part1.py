res = 0

f = open("input.txt")
for line in f:
    numChars = ''.join(char for char in line if char.isnumeric())
    val = numChars[0] + numChars[len(numChars)-1]
    res += int(val)

print(res)