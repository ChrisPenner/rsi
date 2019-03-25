module Parsing.AST where

import Control.Monad.Free
import Operators.Combinators
import Data.Text

data Pipeline
    = Re Text
    | Sh Text [Text]

    deriving (Show, Eq)
