module CamelCards (challengePair) where

import Data.ByteString (ByteString)
import Data.Char (isDigit, digitToInt)
import Data.FileEmbed (embedFile)
import Data.Function (on)
import Data.List (group, sort, sortBy, partition)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge
import Data.Tuple.Extra (both)

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData' challengeData'

data Card
  = Digit Int | T | J | Q | K | A
  deriving (Eq, Ord, Show, Read)

type Hand = (Card, Card, Card, Card, Card)
type Bid = Int
type Input = [(Hand, Bid)]

data HandType
  = High | Pair | TwoPair | Three | FullHouse | Four | Five
  deriving (Eq, Ord, Show)

solve1' :: ByteString -> String
solve1' = solve' compare getType1

solve2' :: ByteString -> String
solve2' = solve' orderCompare2 getType2

solve' :: ([Card] -> [Card] -> Ordering) -> (Hand -> HandType) -> ByteString -> String
solve' orderCompare getType =
  show . sum . zipWith (\rank h -> snd h * rank) [1..] . sortInput orderCompare getType . parseInput

sortInput :: ([Card] -> [Card] -> Ordering) -> (Hand -> HandType) -> Input -> Input
sortInput orderCompare getType = sortBy (compareHands orderCompare getType `on` fst)

compareHands :: ([Card] -> [Card] -> Ordering) -> (Hand -> HandType) -> Hand -> Hand -> Ordering
compareHands orderCompare getType h1 h2 =
  case compare (getType h1) (getType h2) of
    LT -> LT
    GT -> GT
    EQ -> orderCompare (toList h1) (toList h2)

getType1 :: Hand -> HandType
getType1 = get . groupLengths . toList
  where
    get [5] = Five
    get [1, 4] = Four
    get [2, 3] = FullHouse
    get [1, 1, 3] = Three
    get [1, 2, 2] = TwoPair
    get [1, 1, 1, 2] = Pair
    get _ = High

getType2 :: Hand -> HandType
getType2 = get . both groupLengths . partition (/= J) . toList
  where
    get :: ([Int], [Int]) -> HandType
    get ([5], []) = Five
    get ([4], [1]) = Five
    get ([3], [2]) = Five
    get ([2], [3]) = Five
    get ([1], [4]) = Five
    get ([], [5]) = Five
    get ([1, 4], []) = Four
    get ([1, 3], [1]) = Four
    get ([1, 2], [2]) = Four
    get ([1, 1], [3]) = Four
    get ([2, 3], []) = FullHouse
    get ([2, 2], [1]) = FullHouse
    get ([1, 1, 3], []) = Three
    get ([1, 1, 2], [1]) = Three
    get ([1, 1, 1], [2]) = Three
    get ([1, 2, 2], []) = TwoPair
    get ([1, 1, 1, 2], []) = Pair
    get ([1, 1, 1, 1], [1]) = Pair
    get _ = High

groupLengths :: [Card] -> [Int]
groupLengths = sort . (length <$>) . group . sort

orderCompare2 :: [Card] -> [Card] -> Ordering
orderCompare2 h1 h2 = compare (jToLowest <$> h1) (jToLowest <$> h2)
  where
    jToLowest J = Digit (-1)
    jToLowest x = x

toList :: Hand -> [Card]
toList (a, b, c, d, e) = [a, b, c, d, e]

parseInput :: ByteString -> Input
parseInput = (parseLine . T.words <$>) . T.lines . decodeUtf8
  where
    parseLine [h, b] = (parseHand (T.unpack h), read (T.unpack b))
    parseLine _ = error "invalid input"

parseHand :: String -> Hand
parseHand = toQuintuple . (parseCard <$>)
  where
    toQuintuple [a, b, c, d, e] = (a, b, c, d, e)
    toQuintuple _ = error "invalid input"

parseCard :: Char -> Card
parseCard c
  | isDigit c = Digit (digitToInt c)
  | otherwise = read [c]

testData' :: ByteString
testData' = $(embedFile "data/test/7-Cards.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/7-Cards.txt")