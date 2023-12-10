module MirageMaintenance (challengePair) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Ratio (Ratio, (%))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair (solve' (\s -> length s + 1)) (solve' (const 0)) testData' challengeData'

solve' :: ([Int] -> Int) -> ByteString -> String
solve' getIndex = show . sum . ((\s -> round (interpolate s (getIndex s))) <$>) . readInput

interpolate ::  [Int] -> Int -> Ratio Int
interpolate s x =
  sum ((\(j,yj) -> fromIntegral yj * product (map (\m -> (x-m)%(j-m)) (filter (/= j) [1..k]))) <$> zip [1..k] s) where k = length s

readInput :: ByteString -> [[Int]]
readInput = ((read . T.unpack <$>) . T.words <$>) . T.lines . decodeUtf8

testData' :: ByteString
testData' = $(embedFile "data/test/9-Oasis.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/9-Oasis.txt")