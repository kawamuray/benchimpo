module Benchimpo.Util
  ( secsToMicroSecs
  , mvarForkIO
  , mvarForkIOGuard
  ) where

import System.IO
import Control.Concurrent
import Control.Exception

secsToMicroSecs :: (Floating a, RealFrac a) => a -> Int
secsToMicroSecs = floor . (* 10**6)

mvarForkIO' :: (MVar a -> IO b)
            -> (MVar a -> Either SomeException b -> IO ())
            -> IO (MVar a)
mvarForkIO' io finallyFn = do
  mvar <- newEmptyMVar
  _ <- forkFinally (io mvar) $ \st -> do
    case st of
      (Left err) -> hPrint stderr err
      _          -> return ()
    finallyFn mvar st
  return mvar

mvarForkIO :: (MVar a -> IO b) -> IO (MVar a)
mvarForkIO io = mvarForkIO' io (\_ _ -> return ())

mvarForkIOGuard :: IO a -> a -> IO (MVar a)
mvarForkIOGuard io onFailVal = mvarForkIO' (const io) storeMVar
  where storeMVar mvar (Left _) = putMVar mvar onFailVal
        storeMVar mvar (Right v) = putMVar mvar v
