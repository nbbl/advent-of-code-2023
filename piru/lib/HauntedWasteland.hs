module HauntedWasteland (challengePair) where

import Data.Attoparsec.Text (endOfLine, parseOnly, Parser, many', sepBy, letter)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.List (foldl')
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Challenge

challengePair :: ChallengePair
challengePair = ChallengePair solve1' solve2' testData2' challengeData'

data Direction = R | L deriving (Eq, Show, Read)
type NodeId = Text
type Network = Map NodeId (NodeId, NodeId)
type Input = ([Direction], Network)

solve1' :: ByteString -> String
solve1' = show . runMaze "AAA" (== "ZZZ") . readInput

solve2' :: ByteString -> String
solve2' s =
  let input = readInput s
  in  show $ lcmAll $ (\nid -> runMaze nid untilCond input) <$> initialNodes (snd input)
  where
    initialNodes = filter (\n -> lastChar n == 'A') . M.keys
    untilCond n = lastChar n == 'Z'
    lastChar = last . T.unpack
    lcmAll = foldl' lcm 1

runMaze :: NodeId -> (NodeId -> Bool) -> Input -> Int
runMaze initialNode untilCond (directions, network) = go (0, initialNode) (cycle directions)
  where
    go :: (Int, NodeId) -> [Direction] -> Int
    go (steps, currentNode) (dir:dirs)
      | untilCond currentNode = steps
      | otherwise = go (steps + 1, nextNode dir currentNode) dirs
    go _ _ = error "impossible"

    nextNode :: Direction -> NodeId -> NodeId
    nextNode dir nid = let (l, r) = network ! nid in
      case dir of
        L -> l
        R -> r

readInput :: ByteString -> Input
readInput = fromRight . parseOnly parseInput . decodeUtf8

parseInput :: Parser Input
parseInput = (,) <$> (parseDirections <* endOfLine <* endOfLine) <*> parseNetwork

parseDirections :: Parser [Direction]
parseDirections = many' (read . (:[]) <$> letter)

parseNetwork :: Parser Network
parseNetwork = M.fromList <$> parseNode `sepBy` endOfLine

parseNode :: Parser (NodeId, (NodeId, NodeId))
parseNode = toNested <$> (parseNodeId <* " = (") <*> (parseNodeId <* ", ") <*> (parseNodeId <* ")")
  where
    toNested a b c = (a, (b, c))
    parseNodeId = T.pack <$> many' letter

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "expected right"

testData1' :: ByteString
testData1' = $(embedFile "data/test/8-Wasteland-1.txt")

testData2' :: ByteString
testData2' = $(embedFile "data/test/8-Wasteland-2.txt")

challengeData' :: ByteString
challengeData' = $(embedFile "data/challenge/8-Wasteland.txt")