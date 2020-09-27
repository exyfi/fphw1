module HW.Main ( hwMain ) where

import HW.Lang
import HW.Parse
import HW.Print
import HW.Interp
import Text.Megaparsec

hwMain :: IO ()
hwMain = do
    src <- getContents
    case parse parser "input" src of
      Left err -> putStr $ errorBundlePretty err
      Right prog -> putStr $ edsl prog
