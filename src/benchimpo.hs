import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO.Error (ioeGetErrorString)
import Control.Exception (catch)
import qualified Data.List.Split as Split
import Benchimpo.ExprParse
import Benchimpo.Engine
import Benchimpo.Writer
import Benchimpo.Scenario


data Options = Options
  { optVerbose     :: Bool
  , optFunc        :: String
  , optConcurrency :: Int
  , optTimeout     :: Int
  , optClock       :: Double
  , optClockCount  :: Int
  , optOutFile     :: Maybe String
  , optHttpHeader  :: [String]
  , optHttpMethod  :: String
  , optFormData    :: [String]
  }

defaultOptions :: Options
defaultOptions = Options
  { optVerbose     = False
  , optFunc        = "1"
  , optConcurrency = 1
  , optTimeout     = 10
  , optClock       = 1
  , optClockCount  = 1
  , optOutFile     = Nothing
  , optHttpHeader  = []
  , optHttpMethod  = "GET"
  , optFormData    = []
  }

{-# ANN module "HLint: ignore Use string literal" #-}
optionSpecs :: [OptDescr (Options -> Options)]
optionSpecs =
  -- [ Option ['v'] ["verbose"]
  --     (NoArg (\opts -> opts { optVerbose = True }))
  --     "enable verbose output"
  [ Option ['F'] ["fx"]
      (ReqArg (\f opts -> opts { optFunc = f }) "EXPR")
      "function to calculate number of requests at a time"
  , Option ['c'] ["concurrency"]
      (ReqArg (\c opts -> opts { optConcurrency = read c :: Int }) "CONCURRENCY")
      "concurrency level"
  , Option ['t'] ["time"]
      (ReqArg (\t opts -> opts { optClockCount = read t :: Int }) "TIME")
      "clock count to keep issueing requests"
  , Option ['T'] ["timeout"]
      (ReqArg (\t opts -> opts { optTimeout = read t :: Int }) "TIMEOUT")
      "timeout for each request"
  , Option ['C'] ["clock"]
      (ReqArg (\c opts -> opts { optClock = read c :: Double }) "CLOCK")
      "clock for this scenario"
  , Option ['o'] ["output"]
      (ReqArg (\o opts -> opts { optOutFile = Just o }) "FILE")
      "path for file to write result of this scenario"
  , Option ['H'] ["header"]
      (ReqArg (\h opts -> opts { optHttpHeader = optHttpHeader opts ++ [h] }) "HEADER: VALUE")
      "specify an http header to be added for request"
  , Option ['X'] ["method"]
      (ReqArg (\m opts -> opts { optHttpMethod = m }) "GET|POST")
      "specify the method used to send this request"
  , Option ['F'] ["form"]
      (ReqArg (\f opts -> opts { optFormData = optFormData opts ++ [f] }) "NAME=[@<]VALUE")
      "specify a form data to be send with POST request"
  ]

usage :: String
usage = usageInfo header optionSpecs ++ exampleUsage
  where header = "Usage: benchimpo @scenario1 options[ @scenario2 options ...]\nOptions:"
        exampleUsage = unlines
          [ "Example usage:"
          , "  * Force two apps at the same time with requests between 0 ~ 100"
          , "  benchimpo @http://appA.example.com/ \\"
          , "              --fx '(sin(t) + 1) * 100' \\"
          , "              -t 20 \\"
          , "            @http://appB.example.com/ \\"
          , "              --fx '(sin(t) + 1) * 100' \\"
          , "              -t 20"
          , ""
          , "  * Force two apps at different times with requests between 50 ~ 100"
          , "  benchimpo --clock 0.1 \\"
          , "            @http://appA.example.com/ \\"
          , "              --fx '(sin(t) + 1) * 50 + 50' \\"
          , "              -t 20 \\"
          , "            @http://appB.example.com/ \\"
          , "              --fx '(sin(t + 1) + 1) * 50 + 50' \\"
          , "              -t 20"
          , ""
          , "  * Force an app by increasing requests from 1 - 1000 in almost 60 secs"
          , "  benchimpo --clock $((60.0 / 1000)) \\"
          , "    @http://appA.example.com/ --fx 't + 1' -t 999"
          ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions args = case getOpt Permute optionSpecs args of
  (opts, _, []) -> return (foldl (flip id) defaultOptions opts, args)
  (_, _, errs) -> fail $ "Invalid options" ++ show errs


splitArgs :: [String] -> [[String]]
splitArgs args = trim $ Split.split (Split.keepDelimsL matchUrl) (trim args)
  where matchUrl = Split.whenElt (\x -> head x == '@')
        trim = filter (not . null)

mkScenarioWriter :: Maybe String -> IO Writer
mkScenarioWriter Nothing = fail "Option '-o' must supplied for all scenario"
mkScenarioWriter (Just path) = do
  fh <- openFile path WriteMode
  hSetBuffering fh NoBuffering
  return Writer { wrtFh = fh }

parseScenario :: [String] -> IO Scenario
parseScenario (('@':url):opts) = do
  (options, _) <- parseOptions opts
  let fx = optFunc options
  expr <- case parseExpr fx of
    Left err -> fail $ "Can't parse expression '" ++ fx ++ "': " ++ show err
    Right expr -> return expr
  writer <- mkScenarioWriter (optOutFile options)
  return Scenario
    { scUrl         = url
    , scFunc        = expr
    , scConcurrency = optConcurrency options
    , scReqTimeout  = optTimeout options
    , scClock       = optClock options
    , scClockCount  = optClockCount options
    , scWriter      = writer
    }
parseScenario _ = fail "@url specification missing in some scenario"

parseArgs :: [String] -> IO [Scenario]
parseArgs args = mapM parseScenario $ splitArgs args

main :: IO ()
main = do
  args <- getArgs
  scenarios <- parseArgs args `catch` errorExit
  if null scenarios
    then do
      hPutStrLn stderr usage
      exitFailure
    else do
      -- putStrLn $ show scenarios
      bmrs <- run scenarios
      mapM_ finalize $ zip scenarios bmrs
  where errorExit err = do
          hPutStrLn stderr $ "benchimpo: " ++ ioeGetErrorString err
          exitFailure
        finalize (sc, br) = wrtFinalize (scWriter sc) (scUrl sc) br
