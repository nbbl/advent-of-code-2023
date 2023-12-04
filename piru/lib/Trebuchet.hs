module Trebuchet (challengePair) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.ByteString (ByteString)
import Data.Char (isDigit, digitToInt, intToDigit)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Challenge
import Data.Maybe (catMaybes)

challengePair :: ChallengePair
challengePair = ChallengePair (solve' getLineValue1) (solve' getLineValue2) testData' challengeData'

solve' :: (Text -> Int) -> ByteString -> String
solve' getVal = show . sum . map getVal . readData

getLineValue1 :: Text -> Int
getLineValue1 cs =
  let digits = filter isDigit (T.unpack cs)
      digitString = [head digits, last digits]
  in  read digitString

getLineValue2 :: Text -> Int
getLineValue2 cs =
  let digits = fmap intToDigit . fromRight . parseOnly readDigits $ cs
      digitString = [head digits, last digits]
  in  read digitString

readDigits :: Parser [Int]
readDigits = catMaybes <$> many' digitParser

digitParser :: Parser (Maybe Int)
digitParser = (Just . digitToInt <$> digit)
            <|> (Just 1 <$ "one")
            <|> (Just 2 <$ "two")
            <|> (Just 3 <$ "three")
            <|> (Just 4 <$ "four")
            <|> (Just 5 <$ "five")
            <|> (Just 6 <$ "six")
            <|> (Just 7 <$ "seven")
            <|> (Just 8 <$ "eight")
            <|> (Just 9 <$ "nine")
            <|> (Nothing <$ anyChar)

readData :: ByteString -> [Text]
readData = T.lines . decodeUtf8

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "expected right"

--testData' :: ByteString
--testData' = $(embedFile "data/test/1-Trebuchet-1.txt")

testData' :: ByteString
testData' = $(embedFile "data/test/1-Trebuchet-2.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/1-Trebuchet.txt")