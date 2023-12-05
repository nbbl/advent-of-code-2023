import re

f = open("input.txt")

# dictionary to count the number of cards we win
cardCount = {}

for line in f:
    # gets the ID number for the current card
    cardID = int(line[5:8])

    # increments the dictionary count by one for the "original" version of the current card
    cardCount[cardID] = cardCount.get(cardID, 0) + 1

    # gets list of winning numbers and list of the numbers on our card
    splitLine = re.split(': | \| ', line.rstrip())
    winNums, ourNums = re.findall(r'\d+', splitLine[1]), re.findall(r'\d+', splitLine[2])

    # compares winning numbers to our numbers to count the number of winning matches on this card
    winMatches = sum(1 for n in ourNums if n in winNums)

    # starting from the next card and continuing for each winning match on the current card, increment the count
    # of each card by the number of copies we have of the current card
    for card in range(cardID + 1, cardID + winMatches + 1):
        cardCount[card] = cardCount.get(card, 0) + cardCount[cardID]

# sum the counts of all the copies of all the cards
print(sum([x for x in cardCount.values()]))