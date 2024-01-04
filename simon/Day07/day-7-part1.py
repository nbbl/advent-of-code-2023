from collections import Counter

f = open("input.txt")

def getHandType(hand):
    """
    Takes a string representation of a Camel Cards hand and returns a score for that hand type

    Five of a kind = 7
    Four of a kind = 6
    Full House = 5
    Three of a kind = 4
    Two pair = 3
    One pair = 2
    Five distinct cards = 1

    :param hand (str): a text representation of 5 cards where each character represents one card
    :return handScore (int): the score of the hand Type for the input hand ranked 1-7 (7 is best)
    """

    # dictionary with keys representing each possible rank of hand - the numbers describe the frequency of distinct
    # cards in the hand and always add up to 5. e.g. "5" is five of the same card, "122" is two pairs and a fifth
    # non-paired card. the values are a score associated with the strength of each hand type.
    handTypeDict = {"5": 7, "14": 6, "23": 5, "113": 4, "122": 3, "1112": 2, "11111": 1}

    # count the frequency of each card in the hand
    count = Counter(hand)

    # turn the hand into a sorted string of numbers made up of the frequency of each card
    handNums = ''.join([str(y) for y in sorted([x for x in count.values()])])
    return handTypeDict[handNums]

def handToNums(hand):
    """
    Takes a string representation of a Camel Cards hand and converts it to a number for the purpose of finding
    the winning hand between two hands of the same type

    :param hand (str): a text representation of 5 cards where each character represents one card
    :return handNums (int): an integer representation of the input hand
    """

    # create dict to map cards to a number
    handNumsDict = {'1': '01', '2': '02', '3': '03', '4': '04', '5': '05', '6': '06', '7': '07', '8': '08', '9': '09',
                    'T': '10', 'J': '11', 'Q': '12', 'K': '13', 'A': '14'}

    # initialise the res with a 1 to prevent loss of leading zeroes
    handNums = '1'

    # transform cards to their number values, returning it as an integer
    for char in hand:
        handNums += handNumsDict[char]
    return int(handNums)

# for each hand in the input, record the hand along with its bid, hand type (pair, three of a kind etc.), and a
# numerical representation of the hand used to find the stronger of two hands of the same type
handDict = {}
for line in f:
    hand, bid = line.split()[0], int(line.split()[1])
    curHandDict = {"bid": bid, "handType": getHandType(hand), "handNum": handToNums(hand)}
    handDict[hand] = curHandDict

# sort the dictionary by handtype high to low, then by the number representation of the hand, resulting in a list
# of the hands sorted by Rank
sorted_list = sorted(handDict.items(), key=lambda item: (item[1]['handType'], item[1]['handNum']))

# find the winning amount (rank * bid) for each hand and add them to the final result
rank = 0
res = 0
for hand, dict in sorted_list:
    rank += 1
    res += (rank * dict['bid'])

print(res)