module Benchimpo.Engine
  ( run
  ) where

import Control.Concurrent
import Control.Monad (liftM)
import Benchimpo.Benchmark
import Benchimpo.Http
import Benchimpo.Writer
import Benchimpo.Scenario
import Benchimpo.Util ( secsToMicroSecs
                      , mvarForkIO
                      , mvarForkIOGuard)

bmAtom :: IO (Double, Bool) -> IO BmResult
bmAtom fn = do
  (t, success) <- fn
  return BmResult
    { bmReqCount     = 1
    , bmTotalElapsed = t
    , bmMaxElapsed   = t
    , bmMinElapsed   = t
    , bmFailCount    = fcnt success
    }
  where fcnt True = 0
        fcnt False = 1

doRequest :: Scenario -> MVar BmResult -> IO ()
doRequest sc mvar = do
  br0 <- bmAtom $ measureTime $ doGet (scReqTimeout sc) (scUrl sc)
  brs <- takeMVar mvar
  putMVar mvar $ addBmResult br0 brs

bmTask :: Scenario -> (MVar (), Int) -> MVar BmResult -> IO ()
bmTask sc (pistol, nreqs) mvar = do
  readMVar pistol -- Wait until start time if necessary
  mapM_ (const $ doRequest sc mvar) [1..nreqs]

runBmTasks :: Scenario -> [(MVar (), Int)] -> MVar BmResult -> IO ()
runBmTasks sc scheds mvar = mapM_ (flip (bmTask sc) mvar) scheds

aggregator :: [MVar BmResult] -> IO BmResult
aggregator = liftM foldBmResult . mapM drainResult
  where drainResult mvar = do
        br <- takeMVar mvar
        putMVar mvar defaultBmResult
        return br

firePistol :: Maybe (MVar ()) -> IO ()
firePistol (Just pistol) = putMVar pistol ()
firePistol Nothing = return ()

marathon :: Scenario -> Int -> IO BmResult -> [MVar ()] -> IO BmResult
marathon sc totalReqs agg stPistols = drainLoop fillPistols defaultBmResult 0
  where fillPistols = map Just stPistols ++ repeat Nothing
        drainLoop pistols racc i
          | bmReqCount racc == totalReqs = return racc
          | otherwise = do
              firePistol (head pistols)
              threadDelay $ secsToMicroSecs (scClock sc)
              r0 <- agg
              -- putStrLn $ (show $ bmReqCount r0) ++ " requests finished"
              wrtIntState (scWriter sc) curTime nreqsNow r0
              drainLoop (tail pistols) (addBmResult r0 racc) (i + 1)
          where curTime = scClock sc * fromIntegral i
                nreqsNow = scNumReqsAt sc (i + 1)

makeSchedules :: Scenario -> Int -> Int -> [Int]
makeSchedules sc cc idx = [divReqs x + modReqs x idx | x <- [1..lastClk]]
  where lastClk = scClockCount sc
        nreqs = scNumReqsAt sc
        divReqs t = nreqs t `div` cc
        modReqs t i
          | nreqs t `mod` cc >= i = 1
          | otherwise = 0

runScenario :: Scenario -> IO BmResult
runScenario sc = do
  wrtInit (scWriter sc)
  startPistols <- mapM (const newEmptyMVar) [1..(scClockCount sc)]
  workers <- mapM (mvarForkIO . runBmTasks sc . zip startPistols) schedules
  mapM_ (`putMVar` defaultBmResult) workers
  let totalReqs = sum $ concatMap id schedules
  marathon sc totalReqs (aggregator workers) startPistols
  where cc = scConcurrency sc
        schedules = map (makeSchedules sc cc) [1..cc]

run :: [Scenario] -> IO [BmResult]
run = (>>= mapM takeMVar) . mapM execScenario
  where failResult = defaultBmResult { bmFailCount = 1 }
        execScenario sc = mvarForkIOGuard (runScenario sc) failResult
