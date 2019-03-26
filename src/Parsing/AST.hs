{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing.AST where

import Control.Monad.Free
import Operators.Combinators
import Data.Text

type Pipeline = Free PipelineF ()

data PipelineF r
    = Re Text r
    | AddRe Text r
    | RemoveRe Text r
    | Sh Text [Text] r
    | ShSub Text [[Either () Text]] r
    | Map r r
    | Filter r
    | Each r r
    deriving (Eq, Show, Functor)

deriving instance {-# OVERLAPPING #-} (forall a. Show a => Show (PipelineF a)  => Show (Free PipelineF a))
deriving instance {-# OVERLAPPING #-} (forall a. Eq a => Eq (PipelineF a)  => Eq (Free PipelineF a))

re' :: Text -> Free PipelineF ()
re' pat = liftF $ Re pat ()

addRe' :: Text -> Free PipelineF ()
addRe' pat = liftF $ AddRe pat ()

removeRe' :: Text -> Free PipelineF ()
removeRe' pat = liftF $ RemoveRe pat ()

sh' :: Text -> [Text] -> Free PipelineF ()
sh' cmd args = liftF $ Sh cmd args ()

shSub' :: Text -> [[ Either () Text ]] -> Free PipelineF ()
shSub' cmd args = liftF $ ShSub cmd args ()

map' :: Free PipelineF () -> Free PipelineF ()
map' p = Free $ Map p (Pure ())

filter' :: Free PipelineF ()
filter' = liftF $ Filter ()

each' :: Free PipelineF () -> Free PipelineF ()
each' p = Free $ Each p (Pure ())


-- action :: Pipeline
-- action = re' "abc" >> sh' "blah" [] >> map' (re' "thing")
