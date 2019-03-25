{-# LANGUAGE ViewPatterns #-}
module Selectors where


-- import Text.Regex.TDFA.Text hiding (Regex)
-- import Text.Regex.TDFA hiding (Regex)
-- import Text.Regex.PCRE.Text
import Text.Regex.PCRE.Heavy
import Text.Regex.PCRE.Light
import Data.BitVector as BV
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Char as C
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Bool
import Data.Functor.Selection
import Data.Text
import UnliftIO.Process

type S = Selection [] Text Text
type Selector = Text -> Selection [] Text Text
type Editor = Text -> Text
type Eacher = [Text] -> [Text]
type Expander = Selection [] Text Text -> Selection [] Text Text

selecting ::  Selector -> S -> IO S
selecting f s = pure (s >>= f)

editing ::  Editor -> S -> IO S
editing f = pure . fmap f

eaching ::  Eacher -> S -> IO S
eaching f s = pure $ (partsOf traversed %~ f) s

shelling :: Text -> [Text] -> S -> IO S
shelling prog args = traverse (fmap pack . readProcess (unpack prog) (unpack <$> args) . unpack)

re :: Text -> Selector
re pattern txt =
    let matches = fst <$> scanRanges re' txt
        (t, (_, pairs)) = flip runState (0, []) $ foldM go txt matches >>= \t -> unless (T.null t) (_2 <>= [Left t])
     in Selection pairs
  where
    re' :: Regex
    re' = compile (T.encodeUtf8 pattern) []
    go :: Text -> (Int, Int) -> State (Int, [Either Text Text]) Text
    go t (start, end) = do
        let len = end - start
        m <- use _1
        let (before, after) = T.splitAt (start - m) t
        unless (T.null before) $ _2 <>= [Left before]
        let (selected, next) = T.splitAt len after
        unless (T.null selected) $ _2 <>=  [Right selected]
        _1 .= start + len
        return next

upperCaser :: Editor
upperCaser = T.map C.toUpper

collapse :: Selection [] Text Text -> IO Text
collapse = pure . T.concat . forgetSelection

filtered :: Selection [] Text Text -> IO Text
filtered = pure . T.concat . getSelected

sort' :: Eacher
sort' = L.sort
