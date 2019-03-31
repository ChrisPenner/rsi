{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module App where

import           Brick
import           Brick.Markup
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Control.Monad.IO.Class
import           UnliftIO.Exception
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
import Data.Maybe

data S = S { editor'  :: Editor T.Text T.Text
           , contents :: T.Text
           , rendered :: Selection [] T.Text T.Text
           , err      :: Maybe T.Text
           }
    deriving Show

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
           , err = Nothing
           }

renderApp :: S -> [Widget T.Text]
renderApp S {rendered = r, editor' = e, contents = c, err = err'} =
    [editLine <=> errWidget <=> txtWidget]
  where
    t         = fold $ getEditContents e

    errWidget = case err' of
        Just errText -> markup (errText @? "error") <=> hBorder
        Nothing      -> emptyWidget

    txtWidget
        | T.null t = txt c
        | otherwise = markup . fold $ unify (@? "unselected") (@? "selected") r

    editLine = renderEditor (txt.head) True e <=> hBorder

handleEvent :: S -> BrickEvent T.Text e -> EventM T.Text (Next S)
handleEvent s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
handleEvent s (VtyEvent evt) = do
    e' <- handleEditorEvent evt (editor' s)
    let handler e = pure
            $ s { editor' = e'
                , err     = Just . T.pack $ displayException e
                }
    let pipelineTxt = fold $ getEditContents e'
    s' <- liftIO $ flip catchAny handler $ do
        p <- either (throwString . T.unpack) pure $ parsePipeline pipelineTxt
        out <- runPipeline' p (pure $ contents s)
        return
            $ s { editor'  = e'
                , rendered = out
                , err      = Nothing
                }
    continue s'
handleEvent s _ = continue s

attrs :: AttrMap
attrs = attrMap defAttr
                [ ("selected", black `on` white)
                , ("unselected", white `on` black)
                , ("error", red `on` black)
                ]

app :: App S () T.Text
app =
  App { appDraw         = renderApp
      , appChooseCursor = \_ cs -> listToMaybe cs
      , appHandleEvent  = handleEvent
      , appStartEvent   = return
      , appAttrMap      = const attrs
      }
