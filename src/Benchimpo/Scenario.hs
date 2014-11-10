module Benchimpo.Scenario where

import Benchimpo.Expr (Expr, evalExpr)
import Benchimpo.Writer

data Scenario = Scenario
                { scUrl         :: String
                , scFunc        :: Expr
                , scConcurrency :: Int
                , scReqTimeout  :: Int
                , scClock       :: Double
                , scClockCount  :: Int
                , scWriter      :: Writer
                }
  deriving (Show)

scNumReqsAt :: Scenario -> Int -> Int
scNumReqsAt sc clock
  | clock <= scClockCount sc = max 0 val
  | otherwise = 0
  where val = floor $ evalExpr (scFunc sc) (fromIntegral clock)
