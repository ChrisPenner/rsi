module Parsing.AST where

import Control.Monad.Free
import Operators.Combinators
import Data.Text

data Pipeline
    = Re Text
    | Sh Text [Text]
    | Map [Pipeline]

    deriving (Show, Eq)
