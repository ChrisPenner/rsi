{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Interp.Run where


import Control.Exception hiding (throwIO)
import Control.Lens hiding (re)
import Control.Monad
import Control.Monad
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Reader
import Data.Foldable
import Data.Functor.Selection
import Data.List as L
import Operators.Combinators
import Operators.Re
import Parsing.AST
import System.Exit
import UnliftIO.Process
import UnliftIO.Exception
import qualified Data.Text as T
type P a = IO a

eachingP ::  ([T.Text] -> P [T.Text]) -> Ctx -> P Ctx
eachingP f s = (partsOf traversed %%~ f) s

shellingP :: T.Text -> [T.Text] -> Ctx -> P Ctx
shellingP prog args ctx = do
  traverse (runOrError prog args) ctx

runOrError :: T.Text -> [ T.Text ] -> T.Text -> P T.Text
runOrError prog args txt = do
      (code, out, err) <- readProcessWithExitCode (T.unpack prog) (T.unpack <$> args) (T.unpack txt)
      case code of
        ExitSuccess -> return (T.pack out)
        ExitFailure _ -> throwString err

shellSubbingP :: T.Text -> [[Either () T.Text]] -> Ctx -> P Ctx
shellSubbingP prog args ctx = traverse run ctx
    where
        run :: T.Text -> P T.Text
        run txt = runOrError prog (T.concat . fmap (either (const txt) id) <$> args) ""

runPipeline :: Pipeline -> T.Text -> P T.Text
runPipeline p txt = fmap collapse $ interp p (pure txt)

runPipeline' :: Pipeline -> Ctx -> P Ctx
runPipeline' p ctx = interp p ctx

overCtx :: (Ctx -> Ctx) -> Ctx -> P Ctx
overCtx = fmap pure

overCtxIO :: (Ctx -> P Ctx) -> Ctx -> P Ctx
overCtxIO f ctx = f ctx

lifting :: Pipeline -> Ctx -> P Ctx
lifting p pre = traverse (lifting' p) pre
  where
    lifting' :: Pipeline -> T.Text -> P T.Text
    lifting' f txt =
      let lifted = newSelection.pure $ txt
      in fmap collapse.interp f $ lifted

interp :: Pipeline -> Ctx -> P Ctx
interp (Free (Re pat next)) ctx =
  overCtx (selecting (re pat)) ctx >>= interp next
interp (Free (AddRe pat next)) ctx = do
    overCtx (adding (re pat)) ctx >>= interp next
interp (Free (RemoveRe pat next)) ctx = do
    overCtx (removing (re pat)) ctx >>= interp next
interp (Free (Sh cmd args next)) ctx = do
  overCtxIO (shellingP cmd args) ctx >>= interp next
interp (Free (ShSub cmd args next)) ctx = do
    overCtxIO (shellSubbingP cmd args) ctx >>= interp next
interp (Free (Map p next)) ctx = do
  lifting p ctx >>= interp next
interp (Free (Each p next)) ctx = do
    let go :: [T.Text] -> P [T.Text]
        go (T.unlines -> txt) = fmap (L.concat . fmap T.lines . getSelected) $ interp p $ pure txt
    overCtxIO (eachingP go) ctx >>= interp next
interp (Free (Filter next)) ctx = do
    overCtx (newSelection . getSelected) ctx >>= interp next
interp (Pure ()) ctx = return ctx
