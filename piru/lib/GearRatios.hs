module GearRatios (challengePair) where

import Data.ByteString (ByteString)
import Data.Char (isDigit, digitToInt)
import Data.FileEmbed (embedFile)
import Data.List (foldl')
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData' challengeData'

data Grid = Grid Int Int [[Char]] deriving (Eq, Show)
data Number = Number { val_ :: Int, row_ :: Int, start_ :: Int, end_ :: Int } deriving (Eq, Show)

solve1' :: ByteString -> String
solve1' = show . getPartSum . readGrid

solve2' :: ByteString -> String
solve2' = show . sum . gearRatios . readGrid

getPartSum :: Grid -> Int
getPartSum = sum . (val_ <$>) . getPartNumbers

getPartNumbers :: Grid -> [Number]
getPartNumbers grid = filter (numberNextToSymbol grid) $ getNums grid

numberNextToSymbol :: Grid -> Number -> Bool
numberNextToSymbol (Grid _ _ array) num =
  any (nextToSymbol array . (row_ num,)) [start_ num .. end_ num]

nextToSymbol :: [[Char]] -> (Int, Int) -> Bool
nextToSymbol array rowAndCol = any isSymbol (neighbors array rowAndCol)
  where
    isSymbol :: Maybe Char -> Bool
    isSymbol Nothing = False
    isSymbol (Just '.') = False
    isSymbol (Just c) = not (isDigit c)

neighbors :: [[Char]] -> (Int, Int) -> [Maybe Char]
neighbors array (rowI, colI) = lookupGrid array <$> neighborCells (rowI, colI)

neighborCells :: (Int, Int) -> [(Int, Int)]
neighborCells (rowI, colI) =
  [ (rowI - 1, colI)
  , (rowI -1, colI + 1)
  , (rowI, colI + 1)
  , (rowI + 1, colI + 1)
  , (rowI + 1, colI)
  , (rowI + 1, colI - 1)
  , (rowI, colI - 1)
  , (rowI - 1, colI - 1)
  ]

lookupGrid :: [[Char]] -> (Int, Int) -> Maybe Char
lookupGrid array (rowI, colI) =
  if rowI < 1 || rowI > length array
  then Nothing
  else let row = array !! (rowI - 1)
       in if colI < 1 || colI > length row
          then Nothing
          else Just (row !! (colI - 1))

gearRatios :: Grid -> [Int]
gearRatios grid =
  let nums = getNums grid
      asterisks = getAsterisks grid
      gridNums = filter (\ns -> length ns == 2) (neighborNums nums <$> asterisks)
  in  (\numPair -> val_ (head numPair) * val_ (numPair !! 1)) <$> gridNums

getAsterisks :: Grid -> [(Int, Int)]
getAsterisks (Grid rows cols array) = do
  rowI <- [1 .. rows]
  colI <- [1 .. cols]
  [(rowI, colI) | lookupGrid array (rowI, colI) == Just '*']

neighborNums :: [Number] -> (Int, Int) -> [Number]
neighborNums numbers rowAndCol =
  filter (\num -> any (\col -> (row_ num, col) `elem` neighborCells rowAndCol) [start_ num .. end_ num]) numbers

getNums :: Grid -> [Number]
getNums (Grid rows cols array) = [1..rows] >>= getRowNums array cols

getRowNums :: [[Char]] -> Int -> Int -> [Number]
getRowNums array rowLen rowIndex =
  let row = array !! (rowIndex-1)
  in  fst $ foldl' collectNums ([], Nothing) (zip [1..] row)
  where
    collectNums :: ([Number], Maybe (Int, Int)) -> (Int, Char) -> ([Number], Maybe (Int, Int))
    collectNums (previousNums, Nothing) (col, c)
      | isDigit c = if col == rowLen
                    then (previousNums ++ [Number (digitToInt c) rowIndex col col], Nothing)
                    else (previousNums, Just (col, digitToInt c))
      | otherwise = (previousNums, Nothing)
    collectNums (previousNums, Just (start, currentNum)) (col, c)
      | isDigit c = let newNum = currentNum * 10 + digitToInt c
                    in  if col == rowLen
                        then (previousNums ++ [Number newNum rowIndex start col], Nothing)
                        else (previousNums, Just (start, newNum))
      | otherwise = (previousNums ++ [Number currentNum rowIndex start (col-1)], Nothing)

readGrid :: ByteString -> Grid
readGrid s =
  let array = readArray s
  in  Grid (length array) (length (head array)) array

readArray :: ByteString -> [[Char]]
readArray = (T.unpack <$>) . T.lines . decodeUtf8

testData' :: ByteString
testData' = $(embedFile "data/test/3-Gears.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/3-Gears.txt")