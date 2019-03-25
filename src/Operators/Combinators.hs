module Operators.Combinators where

import Data.Text as T
import Data.Functor.Selection
import Control.Arrow
import Control.Lens
import UnliftIO.Process

type Ctx = Selection [] Text Text


type Selector = Text -> Ctx
type Editor = Text -> Text
type Eacher = [Text] -> [Text]
type Expander = Ctx -> Ctx

selecting ::  (Text -> Ctx) -> Ctx -> Ctx
selecting = (=<<)

mapping ::  Editor -> Ctx -> Ctx
mapping =  fmap

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
