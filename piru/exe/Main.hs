module Main (main) where

import System.Environment (getArgs)
import Challenge
import qualified Trebuchet
import qualified CubeConundrum

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
selectChallenge s = error $ "invalid challenge: " <> s
