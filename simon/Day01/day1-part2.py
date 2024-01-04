from word2number import w2n

res = 0
digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

f = open("input.txt")
for line in f:
    numChars = ''
    for i in range(len(line)):
        if line[i].isnumeric():
            numChars += line[i]
        for digitWord in digitWords:
            if line[i:i+len(digitWord)] == digitWord:
                numChars += str(w2n.word_to_num(digitWord))
    val = numChars[0] + numChars[len(numChars)-1]
    res += int(val)

print(res)
