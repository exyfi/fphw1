module HW.Main ( hwMain ) where

import HW.Lang
import HW.Parse
import HW.Print
import HW.Interp
import Text.Megaparsec
import System.Environment
import System.IO
import System.Exit

usage :: IO ()
usage = do
    name <- getProgName
    putStrLn $ "Usage: " <> name <> " <print|edsl|run> <filename>"

load :: FilePath -> IO Block
load filename = do
    src <- readFile filename
    case parse parser "input" src of
      Left err -> do
          putStr $ errorBundlePretty err
          exitFailure
      Right prog -> return prog

actionPrint :: Block -> IO ()
actionPrint = putStr . pretty

actionEdsl :: Block -> IO ()
actionEdsl = putStr . edsl

actionRun :: Block -> IO ()
actionRun = runInterp . interp

args :: [String] -> IO ()
args ["print", filename] = load filename >>= actionPrint
args ["edsl", filename] = load filename >>= actionEdsl
args ["run", filename] = load filename >>= actionRun
args _ = usage

hwMain :: IO ()
hwMain = getArgs >>= args
