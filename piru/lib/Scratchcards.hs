module Scratchcards (challengePair) where

import Data.Attoparsec.Text hiding (take)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM
import Data.IntSet (IntSet, intersection)
import qualified Data.IntSet as IS
import Data.List (foldl', tails)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData' challengeData'

data Card = Card { _id :: Int, _winners :: IntSet, _owned :: IntSet } deriving (Eq, Show)

solve1' :: ByteString -> String
solve1' = show . sum . (cardPoints <$>) . readCards

solve2' :: ByteString -> String
solve2' = show . sum . getCopies . readCards

cardPoints :: Card -> Int
cardPoints card =
  let numMatches = getNumMatches card
  in  if numMatches == 0 then 0 else 2^(numMatches - 1)

getNumMatches :: Card -> Int
getNumMatches = IS.size . matches

matches :: Card -> IntSet
matches card = _owned card `intersection` _winners card

getCopies :: [Card] -> IntMap Int
getCopies cards = foldl' collect originals (zip cards (tail (tails cards)))
  where
    originals :: IntMap Int
    originals = foldl' (\im card -> IM.insert (_id card) 1 im) IM.empty cards

    collect :: IntMap Int -> (Card, [Card]) -> IntMap Int
    collect copies (currCard, nextCards) =
      let cardCopies = copies ! _id currCard
          numMatches = getNumMatches currCard
      in  foldl' (update cardCopies) copies (take numMatches nextCards)

    update :: Int -> IntMap Int -> Card -> IntMap Int
    update toAdd copies card = IM.adjust (+ toAdd) (_id card) copies

readCards :: ByteString -> [Card]
readCards = (fromRight . parseOnly cardParser <$>) . T.lines . decodeUtf8

cardParser :: Parser Card
cardParser = Card <$> ("Card" *> many' space *> decimal <* ":") <*> intSetParser <*> ("|" *> intSetParser)

intSetParser :: Parser IntSet
intSetParser = IS.fromList <$> intsParser

intsParser :: Parser [Int]
intsParser = many' (many' space *> decimal <* many' space)

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "expected right"

testData' :: ByteString
testData' = $(embedFile "data/test/4-Scratchcards.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/4-Scratchcards.txt")