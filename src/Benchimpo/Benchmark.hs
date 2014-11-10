module Benchimpo.Benchmark where

import Data.Time

data BmResult =
  BmResult
  { bmReqCount     :: Int    -- The number of requests represented by this data
  , bmFailCount    :: Int    -- The number of requests that has failed
  , bmTotalElapsed :: Double -- The sum of elapsed time to issue requests
  , bmMaxElapsed   :: Double -- The maximum time needed to issue request
  , bmMinElapsed   :: Double -- The minimum time needed to issue request
  }
  deriving (Show)

defaultBmResult :: BmResult
defaultBmResult = BmResult
  { bmReqCount     = 0
  , bmFailCount    = 0
  , bmTotalElapsed = 0
  , bmMaxElapsed   = 0
  , bmMinElapsed   = 0
  }

addBmResult :: BmResult -> BmResult -> BmResult
addBmResult sc1 sc2 = sc1
  { bmReqCount     = bmReqCount sc1 + bmReqCount sc2
  , bmFailCount    = bmFailCount sc1 + bmFailCount sc2
  , bmTotalElapsed = bmTotalElapsed sc1 + bmTotalElapsed sc2
  , bmMaxElapsed   = max (bmMaxElapsed sc1) (bmMaxElapsed sc2)
  , bmMinElapsed   = minimal (bmMinElapsed sc1) (bmMinElapsed sc2)
  }
  where minimal m1 m2
          | m1 /= 0 && m2 /= 0 = min m1 m2
          | m1 == 0            = m2
          | otherwise          = m1

foldBmResult :: [BmResult] -> BmResult
foldBmResult brs = foldl addBmResult defaultBmResult effects
  where effects = filter ((/= 0) . bmReqCount) brs

bmMeanElapsed :: BmResult -> Double
bmMeanElapsed br = total / nreqs
  where total = bmTotalElapsed br
        nreqs = fromIntegral (bmReqCount br - bmFailCount br)

bmReqPerSec :: BmResult -> Double
bmReqPerSec br = 1 / bmMeanElapsed br

measureTime :: IO a -> IO (Double, a)
measureTime fn = do
  t0 <- getCurrentTime
  res <- fn
  te <- getCurrentTime
  return (realToFrac $ diffUTCTime te t0, res)
