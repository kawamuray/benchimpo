module Benchimpo.Writer where

import System.IO
import Data.List (intercalate)
import Text.Printf (printf)
import Benchimpo.Benchmark

data Writer = Writer
              { wrtFh :: Handle
              }
  deriving (Show)

fields :: [(String, BmResult -> String)]
fields =
  [ ("Count",    w "%d"   bmReqCount)    -- Number of requests made at this time(including failed)
  , ("Failed",   w "%d"   bmFailCount)   -- Number of requests failed
  , ("MaxTime",  w "%.8f" bmMaxElapsed)  -- Maximum time needed for request
  , ("MinTime",  w "%.8f" bmMinElapsed)  -- Minimum time needed for request
  , ("MeanTime", w "%.8f" bmMeanElapsed) -- Mean time needed for request
  , ("Req/sec",  w "%.8f" bmReqPerSec)   -- Request per sec at this time
  ]
  where w fmt = (printf fmt .)

wrtPrintLn :: Writer -> String -> IO ()
wrtPrintLn (Writer fh) = hPutStrLn fh

wrtPrintFields :: Writer -> [String] -> IO ()
wrtPrintFields wrt = wrtPrintLn wrt . intercalate "\t"

wrtInit :: Writer -> IO ()
wrtInit = flip wrtPrintFields $ ["# Clock", "Count"] ++ map fst fields

wrtIntState :: Writer -> Double -> Int -> BmResult -> IO ()
wrtIntState wrt time nreqs br =
  wrtPrintFields wrt $ [show time, show nreqs] ++ [f br | (_, f) <- fields]

wrtFinalize :: Writer -> String -> BmResult -> IO ()
wrtFinalize (Writer fh) url br = do
  hClose fh
  hPutStr stderr $ unlines $ ('@':url) : map ("  " ++) contents
  where join (label, fn) = label ++ ": " ++ fn br
        contents = map join fields
