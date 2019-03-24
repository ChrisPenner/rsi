{-# LANGUAGE ViewPatterns #-}
module Selectors where


import Text.Regex.TDFA.Text hiding (Regex)
import Text.Regex.TDFA hiding (Regex)
import Text.Regex.PCRE.Text
import Data.String as T
import Data.BitVector as BV
import Data.List as L
import Data.Text as T
import qualified Data.Char as C
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Bool
import Data.Functor.Selection

type S = Selection [] Text Text
type Selector = Text -> Selection [] Text Text
type Editor = Text -> Text
type Eacher = [Text] -> [Text]
type Expander = Selection [] Text Text -> Selection [] Text Text

selecting :: S -> Selector -> S
selecting = (>>=)

editing :: S -> Editor -> S
editing = flip fmap

reS :: Text -> Selector
reS re txt =
    let AllMatches matches = txt =~ re :: AllMatches [] (MatchOffset, MatchLength)
        (t, (_, pairs)) = flip runState (0, []) $ foldM go txt matches >>= \t -> unless (T.null t) (_2 <>= [Left t])
     in Selection pairs
  where
    re' :: Regex
    re' = makeRegexOpts compExtended defaultExecOpt re
    go :: Text -> (MatchOffset, MatchLength) -> State (Int, [Either Text Text]) Text
    go t (offset, len) = do
        m <- use _1
        let (before, after) = T.splitAt (offset - m) t
        unless (T.null before) $ _2 <>= [Left before]
        let (selected, next) = T.splitAt len after
        unless (T.null selected) $ _2 <>=  [Right selected]
        _1 .= offset + len
        return next

compS :: Selector -> Selector -> Selector
compS f g txt = f txt >>= g

upperCaser :: Editor
upperCaser = T.map C.toUpper

eacher :: Eacher -> Selection [] Text Text -> Selection [] Text Text
eacher f = partsOf traversed %~ f

collapse :: Selection [] Text Text -> Text
collapse = T.concat . forgetSelection

sort' :: Eacher
sort' = L.sort
