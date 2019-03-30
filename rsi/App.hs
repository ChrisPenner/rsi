{-# LANGUAGE OverloadedStrings #-}
module App where

import           Brick
import           Brick.Markup
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor.Selection
import           Data.Text.IO              as TIO
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Interp.Run
import           Parsing.Parser
import           System.Environment
import           System.Exit
import           System.IO
import qualified Data.Text                 as T

data S =
  S { editor'  :: Editor T.Text T.Text
    , contents :: T.Text
    , rendered :: Selection [] T.Text T.Text
    } deriving Show

usage :: T.Text
usage = "usage: rsi src"

init :: IO S
init = do
  args <- getArgs
  src <- case args of
    [arg] -> pure arg
    _ -> TIO.hPutStrLn stderr usage >> exitFailure
  txt <- TIO.readFile src
  return S { editor'  = editor "cli" (Just 1) ""
           , contents = txt
           , rendered = pure txt
           }

renderApp :: S -> [Widget T.Text]
renderApp S {rendered = r, editor' = e, contents=c} = [editLine <=> hBorder <=> txtWidget]
  where
    t         = fold $ getEditContents e

    txtWidget
      | T.null t = txt c
      | otherwise = markup.fold $ unify (@? "unselected") (@? "selected") r

    editLine = renderEditor (txt.head) True e

handleEvent :: S -> BrickEvent T.Text e -> EventM T.Text (Next S)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent evt) = do
  e' <- handleEditorEvent evt (editor' s)
  let pipelineTxt = fold $ getEditContents e'
  case parsePipeline pipelineTxt of
    Left _ -> continue s{editor'=e'}
    Right p -> do
      out <- liftIO $ runPipeline' p (pure $ contents s)
      continue s { editor' = e'
                 , rendered = out
                }
handleEvent s _ = continue s

attrs :: AttrMap
attrs =
  attrMap defAttr
          [("selected", black `on` white), ("unselected", white `on` black)]

app :: App S () T.Text
app =
  App { appDraw         = renderApp
      , appChooseCursor = \_ _ -> Nothing
      , appHandleEvent  = handleEvent
      , appStartEvent   = return
      , appAttrMap      = const attrs
      }
