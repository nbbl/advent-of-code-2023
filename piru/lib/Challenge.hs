module Challenge
  ( Challenge(..)
  , Part(..)
  , ChallengePair(..)
  , getPart
  ) where

import Data.ByteString (ByteString)

data Challenge = Challenge
  { solve :: ByteString -> String
  , testData :: ByteString
  , challengeData :: ByteString
  }

data Part = Part1 | Part2

data ChallengePair = ChallengePair
  { solve1 :: ByteString -> String
  , solve2 :: ByteString -> String
  , testData :: ByteString
  , challengeData :: ByteString
  }

getPart :: ChallengePair -> Part -> Challenge
getPart cp Part1 = Challenge cp.solve1 cp.testData cp.challengeData
getPart cp Part2 = Challenge cp.solve2 cp.testData cp.challengeData