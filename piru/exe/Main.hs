module Main (main) where

import System.Environment (getArgs)
import Challenge
import qualified Trebuchet
import qualified CubeConundrum
import qualified GearRatios
import qualified Scratchcards
import qualified Seeds
import qualified BoatRace
import qualified CamelCards
import qualified HauntedWasteland

main :: IO ()
main = do
  [challengeString, mode] <- getArgs
  let challenge = selectChallenge challengeString

  case mode of
    "test" -> putStrLn $ challenge.solve challenge.testData
    "challenge" -> putStrLn $ challenge.solve challenge.challengeData
    _ -> error $ "invalid mode " <> mode

selectChallenge :: String -> Challenge
selectChallenge "1.1" = getPart Trebuchet.challengePair Part1
selectChallenge "1.2" = getPart Trebuchet.challengePair Part2
selectChallenge "2.1" = getPart CubeConundrum.challengePair Part1
selectChallenge "2.2" = getPart CubeConundrum.challengePair Part2
selectChallenge "3.1" = getPart GearRatios.challengePair Part1
selectChallenge "3.2" = getPart GearRatios.challengePair Part2
selectChallenge "4.1" = getPart Scratchcards.challengePair Part1
selectChallenge "4.2" = getPart Scratchcards.challengePair Part2
selectChallenge "5.1" = getPart Seeds.challengePair Part1
selectChallenge "5.2" = getPart Seeds.challengePair Part2
selectChallenge "6.1" = getPart BoatRace.challengePair Part1
selectChallenge "6.2" = getPart BoatRace.challengePair Part2
selectChallenge "7.1" = getPart CamelCards.challengePair Part1
selectChallenge "7.2" = getPart CamelCards.challengePair Part2
selectChallenge "8.1" = getPart HauntedWasteland.challengePair Part1
selectChallenge "8.2" = getPart HauntedWasteland.challengePair Part2
selectChallenge s = error $ "invalid challenge: " <> s
