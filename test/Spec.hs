{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import Data.Text as T
import Parsing.AST
import Parsing.Parser
import Text.RawString.QQ
import Interp.Run

parseCheck :: Text ->  [Pipeline] -> IO ()
parseCheck pattern expOutput = do
    case parsePipeline pattern of
      Left err -> expectationFailure err
      Right out -> out `shouldBe` expOutput

runOn :: Text -> Text -> Text -> IO ()
runOn pattern inp expOutput =
    case parsePipeline pattern of
      Left err -> expectationFailure err
      Right pipe -> do
          out <- runPipeline pipe inp
          out `shouldBe` expOutput


main :: IO ()
main = hspec $ do
    describe "parser" $ do
        describe "commands" $ do
            specify "~" $ do
                parseCheck "~ 'a'" [Re "a"]
            specify "!" $ do
                parseCheck "! tr a-z A-Z" [Sh "tr" ["a-z", "A-Z"]]
            specify "%" $ do
                parseCheck "%{ ~ 'a' }" [Map [Re "a"]]
                parseCheck "%{ ~ 'a' | %{ ~ 'blah' } }" [Map [Re "a", Map [Re "blah"]]]
            specify "?" $ do
                parseCheck "%{ ~ 'a' | %{ ~ 'blah' } }" [Map [Re "a", Map [Re "blah"]]]
            specify "!{}" $ do
                parseCheck "!{echo a?b d }" [ShSub "echo" [[ Right "a", Left (), Right "b" ], [Right "d"]]]
            -- specify "?" do
                -- parseCheck "?! echo a?b d" [ShSub "echo" [[ Right "a", Left (), Right "b" ], [Right "d"]]]
            -- specify "?{}" do
                -- parseCheck "^(~ 'a' )" [Map [Re "a"]]
        it "should parse pipelines" $ do
            parseCheck "~ 'a' | ~ 'b'" [Re "a", Re "b"]
        it "should handle escaped quotes" $ do
            parseCheck [r|~ 'a\'' | ~ "b\"" |] [Re "a\'", Re "b\""]
        it "should handle escaped quotes" $ do
            parseCheck [r|~ 'a\'' | ~ "b\"" |] [Re "a\'", Re "b\""]
    describe "run" $ do
        describe "commands" $ do
            specify "~" $ do
                "~ 'a' | ! tr a-z A-Z" `runOn` "bab" $ "bAb"
            specify "!" $ do
                "!{echo -n a?b d }" `runOn` "thing" $ "athingb d"
            -- specify "!" $ do
                -- "!{echo -n a?b d }" `runOn` "thing" $ "athingb d"
            -- specify "%" $ do
                -- "~ 'a\\w+' | %{ ~ '.$' | ! tr 'a-z' 'A-Z' } | ! rev | ! tr -d \\n" `runOn` "abc defgh ape" $ "Cba defgh Epa"
