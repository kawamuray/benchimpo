module Benchimpo.Http
  ( doGet
  ) where

import System.Timeout
import Network.HTTP
import Benchimpo.Util (secsToMicroSecs)

doGet :: Int -> String -> IO Bool
doGet tmo url = do
  rsp <- timeout tmout $ simpleHTTP (getRequest url)
  code <- case rsp of
    Just r -> do
      (code, _, _) <- getResponseCode r
      return code
    Nothing -> return 5

  -- putStrLn $ show code
  -- case rsp of
  --   Left err -> putStrLn $ show err
  --   Right res -> putStrLn $ show $ rspCode res
  return $ code /= 5
  where tmout = secsToMicroSecs (fromIntegral tmo :: Double)
