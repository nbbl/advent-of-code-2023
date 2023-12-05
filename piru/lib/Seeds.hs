module Seeds (challengePair) where

import Data.Attoparsec.Text hiding (take)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Function (on)
import Data.List (find, foldl', sortBy)
import Data.List.Extra (chunksOf)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Extra (both)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData' challengeData'

type SeedMapping = Map Text [SourceDest]

data Input =
  Input { _seeds :: [Int], _mapping :: SeedMapping }
  deriving (Eq, Show)

data SourceDest =
  SourceDest { _destStart :: Int, _sourceStart :: Int, _rangeLen :: Int }
  deriving (Eq, Show)

_sourceEnd :: SourceDest -> Int
_sourceEnd sd = _sourceStart sd + _rangeLen sd - 1

type Range = (Int, Int)

solve1' :: ByteString -> String
solve1' s =
  let input = readInput s
  in  show $ minimum (seedLocation (_mapping input) <$> _seeds input)

solve2' :: ByteString -> String
solve2' s =
  let input = readInput s
      ranges = (\c -> (head c, head c + c !! 1 - 1)) <$> chunksOf 2 (_seeds input)
      locationRanges = seedLocationRanges (_mapping input) ranges
  in  show $ minimum $ fst <$> locationRanges

seedLocationRanges :: SeedMapping -> [Range] -> [Range]
seedLocationRanges mapping =
  doMap "humidity-to-location" .
  doMap "temperature-to-humidity" .
  doMap "light-to-temperature" .
  doMap "water-to-light" .
  doMap "fertilizer-to-water" .
  doMap "soil-to-fertilizer" .
  doMap "seed-to-soil"

  where
    doMap :: Text -> [Range] -> [Range]
    doMap key = (>>= mapRange (mapping ! key))

mapRange :: [SourceDest] -> Range -> [Range]
mapRange sds range = combine $ foldl' mapSubrange ([], Just range) $ sortBy (compare `on` _sourceStart) sds
  where
    mapSubrange :: ([Range], Maybe Range) -> SourceDest -> ([Range], Maybe Range)
    mapSubrange (result, Nothing) _ = (result, Nothing)
    mapSubrange (ranges, Just (start, end)) sd =
      if _sourceEnd sd < start || _sourceStart sd > end
      then (ranges, Just (start, end))
      else
        let mappedRange = both (\i -> _destStart sd + (i - _sourceStart sd)) (start, min end (_sourceEnd sd))
        in  filterValid (mappedRange : (start, _sourceStart sd - 1) : ranges, Just (_sourceEnd sd + 1, end))

    combine :: ([Range], Maybe Range) -> [Range]
    combine (ranges, Nothing) = ranges
    combine (ranges, Just r) = r : ranges

    filterValid :: ([Range], Maybe Range) -> ([Range], Maybe Range)
    filterValid (rs, mr) = (filter rangeValid rs, mr >>= \r -> if rangeValid r then Just r else Nothing)

    rangeValid :: Range -> Bool
    rangeValid (start, end) = start <= end

seedLocation :: SeedMapping -> Int -> Int
seedLocation mapping =
  doMap "humidity-to-location" .
  doMap "temperature-to-humidity" .
  doMap "light-to-temperature" .
  doMap "water-to-light" .
  doMap "fertilizer-to-water" .
  doMap "soil-to-fertilizer" .
  doMap "seed-to-soil"

  where
    doMap = mapSource mapping

mapSource :: SeedMapping -> Text -> Int -> Int
mapSource mapping category source =
  let categoryMapping = mapping ! category
  in  case findInRange source categoryMapping of
        Nothing -> source
        Just sd -> _destStart sd + (source - _sourceStart sd)

findInRange :: Int -> [SourceDest] -> Maybe SourceDest
findInRange n = find (\sd -> n >= _sourceStart sd && n < _sourceStart sd + _rangeLen sd)

readInput :: ByteString -> Input
readInput = fromRight . parseOnly inputParser . decodeUtf8

inputParser :: Parser Input
inputParser =
  Input <$> (seedsParser <* endOfLine <* endOfLine) <*> (M.fromList <$> many' (singleMappingParser <* option () endOfLine))

seedsParser :: Parser [Int]
seedsParser = "seeds:" *> many' numParser

singleMappingParser :: Parser (Text, [SourceDest])
singleMappingParser =
  (,) . T.pack <$> many' (notChar ' ') <* " map:" <* endOfLine <*> many' (sourceDestParser <* option () endOfLine)

sourceDestParser :: Parser SourceDest
sourceDestParser = SourceDest <$> numParser <*> numParser <*> numParser

numParser :: Parser Int
numParser = many' " " *> decimal <* many' " "

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "expected right"

testData' :: ByteString
testData' = $(embedFile "data/test/5-Seeds.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/5-Seeds.txt")