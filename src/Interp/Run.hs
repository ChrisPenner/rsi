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

runPipeline :: Pipeline -> T.Text -> IO T.Text
runPipeline p txt = fmap collapse . flip execStateT (pure txt) $ interp p
-- runPipeline xs txt = T.concat . forgetSelection <$> interp xs (newSelection [txt])
-- runPipeline :: Free PipelineF Ctx -> T.Text -> IO T.Text
-- runPipeline xs txt = collapse <$> interp xs (newSelection [txt])


-- interp1 :: Pipeline -> Ctx -> IO Ctx
-- interp1 (Re pat) ctx = pure . re pat $ ctx
-- interp1 (Sh cmd args) ctx = shelling cmd args $ ctx
-- interp1 (ShSub cmd args) ctx = shellSubbing cmd args $ ctx
-- interp1 (Map xs) ctx = join <$> sequence thing
--     where
--         thing = do
--             c' <- newSelection . pure <$> ctx
--             return $ interp xs c'



interp :: Pipeline -> StateT Ctx IO ()
interp (Free (Re pat next)) = do
    modify (selecting (re pat))
    interp next
interp (Free (Sh cmd args next)) = do
    get >>= liftIO . shelling cmd args >>= put
    interp next
interp (Free (ShSub cmd args next)) = do
    get >>= liftIO . shellSubbing cmd args >>= put
    interp next
interp (Pure ()) = return ()
interp (Free (Map f next)) = do
    pre <- get
    liftIO (traverse (lifting f) pre) >>= put
    interp next
  where
    lifting :: Pipeline -> T.Text -> IO T.Text
    lifting f txt =
        let lifted = newSelection . pure $ txt
         in fmap collapse . liftIO $ flip execStateT lifted $ interp f

