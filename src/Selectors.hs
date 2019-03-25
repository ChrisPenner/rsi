{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Selectors where


-- import Text.Regex.PCRE.Heavy hiding (re)
-- import Text.Regex.PCRE.Light
-- import Data.List as L
-- import Data.Text as T
-- import Data.Text.Encoding as T
-- import qualified Data.Char as C
-- import Control.Monad
-- import Control.Monad.State
-- import Control.Lens hiding (re, mapping)
-- import Data.Bool
-- import Data.Functor.Selection
-- import Data.Text
-- import UnliftIO.Process
-- import Control.Arrow

-- type S = Selection [] Text Text
-- type Selector = Text -> Selection [] Text Text
-- type Editor = Text -> Text
--
-- type Eacher = [Text] -> [Text]
-- type Expander = Selection [] Text Text -> Selection [] Text Text

-- selecting ::  Selector -> S -> S
-- selecting = (=<<)

-- mapping ::  Editor -> S -> S
-- mapping =  fmap

-- eaching ::  Eacher -> S -> S
-- eaching f s = (partsOf traversed %~ f) s

-- filtering ::  (Text -> Bool) -> Expander
-- filtering p = select p

-- shelling :: Text -> [Text] -> Kleisli IO S S
-- shelling prog args = Kleisli $ traverse (fmap pack . readProcess (unpack prog) (unpack <$> args) . unpack)

-- re ::  Text -> S -> S
-- re = selecting . re'

-- re' :: Text -> Selector
-- re' pattern txt =
--     let matches = fst <$> scanRanges re' txt
--         (t, (_, pairs)) = flip runState (0, []) $ foldM go txt matches >>= \t -> unless (T.null t) (_2 <>= [Left t])
--      in Selection pairs
--   where
--     re' :: Regex
--     re' = compile (T.encodeUtf8 pattern) []
--     go :: Text -> (Int, Int) -> State (Int, [Either Text Text]) Text
--     go t (start, end) = do
--         let len = end - start
--         m <- use _1
--         let (before, after) = T.splitAt (start - m) t
--         unless (T.null before) $ _2 <>= [Left before]
--         let (selected, next) = T.splitAt len after
--         unless (T.null selected) $ _2 <>=  [Right selected]
--         _1 .= start + len
--         return next

-- upperCaser :: Editor
-- upperCaser = T.map C.toUpper

-- collapse :: Selection [] Text Text -> IO Text
-- collapse = pure . T.concat . forgetSelection

-- filtered :: Selection [] Text Text -> IO Text
-- filtered = pure . T.concat . getSelected

-- sort' :: Eacher
-- sort' = L.sort

-- chain :: Kleisli IO S S
-- chain = (re "\\w+") ^>> shelling "sort" []  >>> mapping T.toUpper ^>> shelling "sort" []
