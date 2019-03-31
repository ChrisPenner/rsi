{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (getContents)
import Interp.Run
import Parsing.Parser
import System.IO (stderr)
import System.Exit
import System.Environment
import qualified Data.Text as T
import Data.Text.IO as TIO
import Control.Exception
import Control.Monad
import Control.Monad.Except
import UnliftIO.Exception

usage :: T.Text
usage = "usage: exe <pipeline>"

main :: IO ()
main = flip catchAny handler $ do
    args <- getArgs
    unless (length args == 1) $ hPutStrLn stderr usage >> exitFailure
    contents <- getContents
    pipeline <- either (throwString . T.unpack) pure $ parsePipeline (T.pack $ head args)
    out <- runPipeline pipeline contents
    TIO.putStr out
  where
    handler e = hPutStrLn stderr (T.pack $ displayException e) >> exitFailure

