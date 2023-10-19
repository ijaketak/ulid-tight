module Main where

import Control.DeepSeq (NFData, force)
import Control.Monad (replicateM)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random.Stateful

import Data.Ulid

main :: IO ()
main = do
  let ops = 1000000
  sg <- initStdGen
  ag <- newAtomicGenM sg
  measure ops "getUlid" $ getUlid ag
  t <- getPOSIXTime
  measure ops "randomUlidM" $ randomUlidM t ag

measure :: NFData a => Int -> String -> IO a -> IO ()
measure ops methodName method = do
  begin <- getPOSIXTime
  _ <- replicateM ops $ method >>= return . force
  end <- getPOSIXTime
  let elapsed = end - begin
      opsPerSec = fromIntegral ops / realToFrac elapsed :: Double
  putStrLn $ methodName <> ": " <> show (round opsPerSec :: Int) <> " op/s"
