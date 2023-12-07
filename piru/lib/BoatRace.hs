module BoatRace (challengePair) where

import Data.ByteString (ByteString)
import Data.List (sort, transpose)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData' challengeData'

type Input = [Race]
data Race = Race { _time :: Int, _distance :: Int } deriving (Eq, Show)

data Range
  = Unbounded
  | Below Double
  | Between Double Double
  | Above Double
  deriving (Eq, Show)

solve1' :: ByteString -> String
solve1' = show . product . (solveRace <$>) . readInput1

solve2' :: ByteString -> String
solve2' = show . solveRace . readInput2

solveRace :: Race -> Int
solveRace race =
  length (solveQuadraticGTZeroInts (-1, _time race, -(_distance race)))

solveQuadraticGTZeroInts :: (Int, Int, Int) -> [Int]
solveQuadraticGTZeroInts (a, b, c) =
  (intsInRanges . solveQuadraticGTZero) (fromIntegral a, fromIntegral b, fromIntegral c)
  where
    intsInRanges :: [Range] -> [Int]
    intsInRanges [Between x y] =
      let lower = if fromInteger (ceiling x) == x then ceiling x + 1 else ceiling x
          upper = if fromInteger (floor y) == y then floor y - 1 else floor y
      in  [lower..upper]
    intsInRanges _ = error "impossible or infinite solutions"

solveQuadraticGTZero :: (Double, Double, Double) -> [Range]
solveQuadraticGTZero (a, b, c) = (filter isGTZero . getRanges . solveQuadratic) (a, b, c)
  where
    isGTZero :: Range -> Bool
    isGTZero range = plugIn (rangeTestVal range) > 0

    rangeTestVal :: Range -> Double
    rangeTestVal Unbounded = 0
    rangeTestVal (Below x) = x - 1
    rangeTestVal (Above x) = x + 1
    rangeTestVal (Between x y) = (x + y) / 2

    plugIn :: Double -> Double
    plugIn x = a * (x ** 2) + b * x + c

    getRanges :: [Double] -> [Range]
    getRanges [] = [Unbounded]
    getRanges [x] = [Below x, Above x]
    getRanges [x, y] = [Below x, Between x y, Above y]
    getRanges _ = error "impossible"

solveQuadratic :: (Double, Double, Double) -> [Double]
solveQuadratic (a, b, c)
  | discriminant < 0 = []
  | discriminant == 0 = [(-b) / (2 * a)]
  | otherwise = sort [(-b - sqrt discriminant) / (2 * a), (-b + sqrt discriminant) / (2 * a)]
  where
    discriminant = b * b - 4 * a * c

readInput1 :: ByteString -> Input
readInput1 = (toRace <$>) . transpose . (tail . T.words <$>) . T.lines . decodeUtf8
  
readInput2 :: ByteString -> Race
readInput2 = toRace . (T.concat . tail . T.words <$>) . T.lines . decodeUtf8

toRace :: [Text] -> Race
toRace [t, d] = Race ((read . T.unpack) t) ((read . T.unpack) d)
toRace _ = error "invalid input"

testData' :: ByteString
testData' = $(embedFile "data/test/6-Races.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/6-Races.txt")