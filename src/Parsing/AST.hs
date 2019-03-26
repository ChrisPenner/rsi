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
    | Sh Text [Text] r
    | ShSub Text [[Either () Text]] r
    | Map r r
    | Filter (PipelineF Bool) r
    deriving (Eq, Show, Functor)

deriving instance {-# OVERLAPPING #-} (forall a. Show a => Show (PipelineF a)  => Show (Free PipelineF a))
deriving instance {-# OVERLAPPING #-} (forall a. Eq a => Eq (PipelineF a)  => Eq (Free PipelineF a))

re' :: Text -> Free PipelineF ()
re' pat = liftF $ Re pat ()

sh' :: Text -> [Text] -> Free PipelineF ()
sh' cmd args = liftF $ Sh cmd args ()

shSub' :: Text -> [[ Either () Text ]] -> Free PipelineF ()
shSub' cmd args = liftF $ ShSub cmd args ()

map' :: Free PipelineF () -> Free PipelineF ()
map' p = Free $ Map p (Pure ())

filter' :: PipelineF Bool -> Free PipelineF ()
filter' p = liftF $ Filter p ()

-- action :: Pipeline
-- action = re' "abc" >> sh' "blah" [] >> map' (re' "thing")
