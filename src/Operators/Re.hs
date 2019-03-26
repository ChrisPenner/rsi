{-# LANGUAGE ViewPatterns #-}
module Operators.Re (re) where

import Text.Regex.PCRE.Heavy hiding (re)
import Text.Regex.PCRE.Light
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Char as C
import Control.Monad
import Control.Monad.State
import Control.Lens hiding (re, mapping)
import Data.Bool
import Data.Functor.Selection
import Data.Text
import UnliftIO.Process
import Control.Arrow
import Operators.Combinators

-- re ::  Text -> Ctx -> Ctx
-- re = selecting . re'

re :: Text -> Selector
re pattern txt =
    let matches         = fst <$> scanRanges re' txt
        (t, (_, pairs)) =
            flip runState (0, [])
            $ foldM go txt matches >>= \t -> unless (T.null t) (_2 <>= [Left t])
    in Selection pairs
  where
    re' :: Regex
    re' = compile (T.encodeUtf8 pattern) []

    go :: Text -> (Int, Int) -> State (Int, [Either Text Text]) Text
    go t (start, end) =
        do
            let len = end - start
            m <- use _1
            let (before, after) = T.splitAt (start - m) t
            unless (T.null before) $ _2 <>= [Left before]
            let (selected, next) = T.splitAt len after
            unless (T.null selected) $ _2 <>= [Right selected]
            _1 .= start + len
            return next
