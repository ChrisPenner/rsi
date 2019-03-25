{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (getContents, putStr)
import Interp.Run
import Parsing.Parser
import System.IO (stderr)
import System.Exit
import System.Environment
import qualified Data.Text as T
import Data.Text.IO
import Control.Monad

usage :: T.Text
usage = "usage: exe <pipeline>"

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) $ hPutStrLn stderr usage >> exitFailure
    contents <- getContents
    case parsePipeline (T.pack $ head args) of
      Left err -> hPutStrLn stderr err >> exitFailure
      Right pipeline -> runPipeline pipeline contents >>= putStr
