{-# LANGUAGE ScopedTypeVariables #-}
module Operators.Combinators where

import Data.Text as T
import Data.Functor.Selection
import Control.Arrow
import Control.Lens
import UnliftIO.Process
import Data.List as L

type Ctx = Selection [] Text Text


type Selector = Text -> Ctx
type Editor = Text -> Text
type Eacher = [Text] -> [Text]
type EacherIO = [Text] -> IO [Text]
type Expander = Ctx -> Ctx

selecting ::  (Text -> Ctx) -> Ctx -> Ctx
selecting = (=<<)

adding :: (Text -> Ctx) -> Ctx -> Ctx
adding f = modifySelection (L.concat . fmap go')
    where
        go' :: Either Text Text -> [Either Text Text]
        go' (Left x) = unwrapSelection $ f x
        go' (Right x) = [Right x]

removing :: (Text -> Ctx) -> Ctx -> Ctx
removing f = modifySelection (L.concat . fmap go')
    where
        go' :: Either Text Text -> [Either Text Text]
        go' (Left x) = [Left x]
        go' (Right x) = unwrapSelection $ invertSelection $  f x

mapping ::  Editor -> Ctx -> Ctx
mapping =  fmap

eachingIO ::  EacherIO -> Ctx -> IO Ctx
eachingIO f s = (partsOf traversed %%~ f) s

eaching ::  Eacher -> Ctx -> Ctx
eaching f s = (partsOf traversed %~ f) s


filtering ::  (Text -> Bool) -> Expander
filtering p = select p

-- | TODO make more efficient by grouping up calls;
-- come up with less hacky way to trin newlines
shelling :: Text -> [Text] -> Ctx -> IO Ctx
shelling prog args = traverse (fmap pack . readProcess (unpack prog) (unpack <$> args) . unpack)

shellSubbing :: Text -> [[Either () Text]] -> Ctx -> IO Ctx
shellSubbing prog args = traverse run
    where
        run :: Text -> IO Text
        run txt = pack <$> readProcess (unpack prog) (T.unpack . T.concat . fmap (either (const txt) id) <$> args) ""

collapse :: Ctx -> Text
collapse = T.concat . forgetSelection

filtered :: Ctx -> Text
filtered =  T.concat . getSelected
