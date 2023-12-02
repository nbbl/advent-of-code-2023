module CubeConundrum (challengePair) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData' challengeData'

data Game = Game ID [Map Color Int] deriving (Eq, Show)
type ID = Int
data Color = Blue | Red | Green deriving (Eq, Show, Ord)

getId :: Game -> ID
getId (Game i _) = i

solve1' :: ByteString -> String
solve1' = show . sum . map getId . filter (gameValid maxCubes) . readGames
  where
    maxCubes = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

solve2' :: ByteString -> String
solve2' = show . sum . map minPower . readGames

gameValid :: Map Color Int -> Game -> Bool
gameValid maxCubes (Game _ sets) = all (setValid maxCubes) sets

setValid :: Map Color Int -> Map Color Int -> Bool
setValid maxCubes drawnCubes = all validDraw (M.assocs drawnCubes)
  where
    validDraw :: (Color, Int) -> Bool
    validDraw (color, drawn) = drawn <= maxCubes ! color

minPower :: Game -> Int
minPower game = minOfGame Blue game * minOfGame Red game * minOfGame Green game

minOfGame :: Color -> Game -> Int
minOfGame color (Game _ sets) = maximum $ minOfDraw color <$> sets

minOfDraw :: Color -> Map Color Int -> Int
minOfDraw color draw = fromMaybe 0 $ M.lookup color draw

readGames :: ByteString -> [Game]
readGames = (fmap . fmap) decodeLine readLines

decodeLine :: Text -> Game
decodeLine line =
  let parts = T.splitOn ": " line
      gameId = read . T.unpack $ T.splitOn " " (head parts) !! 1
      setStrings = T.splitOn "; " (parts !! 1)
      sets = decodeSet <$> setStrings
  in  Game gameId sets

decodeSet :: Text -> Map Color Int
decodeSet s = M.fromList (decodeDraw <$> T.splitOn ", " s)

decodeDraw :: Text -> (Color, Int)
decodeDraw s =
  let parts = T.splitOn " " s
  in  (decodeColor (parts !! 1), (read . T.unpack) (head parts))

decodeColor :: Text -> Color
decodeColor "blue" = Blue
decodeColor "red" = Red
decodeColor "green" = Green
decodeColor s = error ("invalid color " ++ T.unpack s)

readLines :: ByteString -> [Text]
readLines = T.lines . decodeUtf8

testData' :: ByteString
testData' = $(embedFile "data/test/2-Cubes.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/2-Cubes.txt")