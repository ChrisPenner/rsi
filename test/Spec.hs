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
            specify "re" $ do
                parseCheck "re 'a'" [Re "a"]
            specify "!" $ do
                parseCheck "! tr a-z A-Z" [Sh "tr" ["a-z", "A-Z"]]
        it "should parse pipelines" $ do
            parseCheck "re 'a' | re 'b'" [Re "a", Re "b"]
        it "should handle escaped quotes" $ do
            parseCheck [r|re 'a\'' | re "b\"" |] [Re "a\'", Re "b\""]
        it "should handle escaped quotes" $ do
            parseCheck [r|re 'a\'' | re "b\"" |] [Re "a\'", Re "b\""]
