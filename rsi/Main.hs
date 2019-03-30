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
import Brick
import App

usage :: T.Text
usage = "usage: rsi"

main :: IO ()
main = do
    s <- App.init
    void $ defaultMain app s
    -- case parsePipeline (T.pack $ head args) of
    --   Left err -> hPutStrLn stderr err >> exitFailure
    --   Right pipeline -> runPipeline pipeline contents >>= putStr
