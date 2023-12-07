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

solve1' :: ByteString -> String
solve1' = show . product . (solveRace <$>) . readInput1

solve2' :: ByteString -> String
solve2' = show . solveRace . readInput2

solveRace :: Race -> Int
solveRace race = numIntsGTQuad (-1) (_time race) (-(_distance race))

numIntsGTQuad :: Int -> Int -> Int -> Int
numIntsGTQuad a b c =
  numIntsInRange (solveQuadratic (fromIntegral a) (fromIntegral b) (fromIntegral c))
  where
    numIntsInRange :: [Double] -> Int
    numIntsInRange [x, y] = max 0 $ ceiling y - floor x - 1
    numIntsInRange _ = error "invalid input"

solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c
  | discriminant > 0 = sort [(-b - sqrt discriminant) / (2 * a), (-b + sqrt discriminant) / (2 * a)]
  | otherwise = error "invalid input"
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