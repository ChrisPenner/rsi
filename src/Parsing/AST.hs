module Parsing.AST where

import Control.Monad.Free
import Operators.Combinators
import Data.Text

data Pipeline
    = Re Text
    | Sh Text [Text]
    | ShSub Text [[Either () Text]]
    | Map [Pipeline]

    deriving (Show, Eq)
