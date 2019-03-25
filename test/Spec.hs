{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import Data.Text as T
import Parsing.AST
import Parsing.Parser
import Text.RawString.QQ

parseCheck :: Text ->  [Pipeline] -> IO ()
parseCheck pattern expOutput = do
    case parsePipeline pattern of
      Left err -> expectationFailure err
      Right out -> out `shouldBe` expOutput

main :: IO ()
main = hspec $
    describe "parser" $ do
        describe "commands" $ do
            specify "~" $ do
                parseCheck "~ 'a'" [Re "a"]
            specify "!" $ do
                parseCheck "! tr a-z A-Z" [Sh "tr" ["a-z", "A-Z"]]
            specify "%" $ do
                parseCheck "%{ ~ 'a' }" [Map [Re "a"]]
                parseCheck "%{ ~ 'a' | %{ ~ 'blah' } }" [Map [Re "a", Map [Re "blah"]]]
            -- specify "^" $ do
                -- parseCheck "^(~ 'a' )" [Map [Re "a"]]
        it "should parse pipelines" $ do
            parseCheck "~ 'a' | ~ 'b'" [Re "a", Re "b"]
        it "should handle escaped quotes" $ do
            parseCheck [r|~ 'a\'' | ~ "b\"" |] [Re "a\'", Re "b\""]
        it "should handle escaped quotes" $ do
            parseCheck [r|~ 'a\'' | ~ "b\"" |] [Re "a\'", Re "b\""]
