module Interp.Run where

import Parsing.AST
import qualified Data.Text as T
import Data.Foldable
import Control.Monad
import Operators.Combinators
import Control.Monad
import Operators.Re
import Data.Functor.Selection

interp :: [Pipeline] -> Ctx -> IO Ctx
interp xs = composeAll $ fmap interp1 xs
  where
      composeAll :: [a  -> IO a] -> a -> IO a
      composeAll = foldl' go pure
      go f g = f >=> g

runPipeline :: [Pipeline] -> T.Text -> IO T.Text
runPipeline xs txt = T.concat . forgetSelection <$> interp xs (newSelection [txt])

interp1 :: Pipeline -> Ctx -> IO Ctx
interp1 (Re pat) ctx = pure . re pat $ ctx
interp1 (Sh cmd args) ctx = shelling cmd args $ ctx
interp1 (ShSub cmd args) ctx = shellSubbing cmd args $ ctx
interp1 (Map xs) ctx = join <$> sequence thing
    where
        thing = do
            c' <- newSelection . pure <$> ctx
            return $ interp xs c'

