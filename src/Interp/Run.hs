{-# LANGUAGE ViewPatterns #-}
module Interp.Run where

import Parsing.AST
import qualified Data.Text as T
import Data.Foldable
import Control.Monad
import Operators.Combinators
import Control.Monad
import Control.Monad.Reader
import Operators.Re
import Data.Functor.Selection
import Control.Monad.Free
import Control.Monad.State
import Data.List as L

runPipeline :: Pipeline -> T.Text -> IO T.Text
runPipeline p txt = fmap collapse . flip execStateT (pure txt) $ interp p

overCtx :: (Ctx -> Ctx) -> StateT Ctx IO ()
overCtx = modify

overCtxIO :: (Ctx -> IO Ctx) -> StateT Ctx IO ()
overCtxIO f = get >>= liftIO . f >>= put

lifting :: Pipeline -> StateT Ctx IO ()
lifting p = do
    pre <- get
    liftIO (traverse (lifting' p) pre) >>= put
  where
    lifting' :: Pipeline -> T.Text -> IO T.Text
    lifting' f txt =
        let lifted = newSelection . pure $ txt
            in fmap collapse . liftIO $ flip execStateT lifted $ interp f

interp :: Pipeline -> StateT Ctx IO ()
interp (Free (Re pat next)) = do
    overCtx (selecting (re pat))
    interp next
interp (Free (AddRe pat next)) = do
    overCtx (adding (re pat))
    interp next
interp (Free (RemoveRe pat next)) = do
    overCtx (removing (re pat))
    interp next
interp (Free (Sh cmd args next)) = do
    overCtxIO $ shelling cmd args
    interp next
interp (Free (ShSub cmd args next)) = do
    overCtxIO $ shellSubbing cmd args
    interp next
interp (Free (Map p next)) = do
    lifting p
    interp next
interp (Free (Each p next)) = do
    ctx <- get
    let go :: [T.Text] -> IO [T.Text]
        go (T.unlines -> txt) = fmap (L.concat . fmap T.lines . getSelected) . execStateT (interp p) $ pure txt
    overCtxIO (eachingIO go)
    interp next
interp (Free (Filter next)) = do
    overCtx (newSelection . getSelected)
    interp next
interp (Pure ()) = return ()
