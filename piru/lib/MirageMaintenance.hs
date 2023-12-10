module MirageMaintenance (challengePair) where

import Combinatorics (binomial)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair (solve' solveSeq1) (solve' solveSeq2) testData' challengeData'

type Sequence = IntMap Int

solve' :: (Sequence -> Int) -> ByteString -> String
solve' solver = show . sum . (solver <$>) . readInput

solveSeq1 :: Sequence -> Int
solveSeq1 s = lastElem + sum ((\red -> reduceSequence s red (reducedLength s red)) <$> [1..constReduction s])
  where
    lastElem = s ! IM.size s

solveSeq2 :: Sequence -> Int
solveSeq2 s = sum $ zipWith (*) (cycle [1, -1]) $ firstElem : ((\red -> reduceSequence s red 1) <$> [1..constReduction s])
  where
    firstElem = s ! 1

constReduction :: Sequence -> Int
constReduction s =
  fromJust $ find (\i -> allSame (reduceSequence s i <$> [1..(reducedLength s i)])) [1..]
  where
    allSame xs = IS.size (IS.fromList xs) == 1

reduceSequence :: Sequence -> Int -> Int -> Int
reduceSequence original reductions i = sum $ zipWith3 go [i..i+reductions] [0..] (cycle signs)
  where
    go :: Int -> Int -> Int -> Int
    go j pascalIndex sign = sign * pascalNum pascalIndex * (original ! j)

    pascalNum :: Int -> Int
    pascalNum = binomial reductions

    signs :: [Int]
    signs
     | even reductions = [1, -1]
     | otherwise = [-1, 1]

reducedLength :: Sequence -> Int -> Int
reducedLength original reductions = IM.size original - reductions

readInput :: ByteString -> [Sequence]
readInput =
  (IM.fromList . zip [1..] . (read . T.unpack <$>) . T.words <$>) . T.lines . decodeUtf8

testData' :: ByteString
testData' = $(embedFile "data/test/9-Oasis.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/9-Oasis.txt")