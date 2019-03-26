{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.Text as T
import Parsing.AST
import Parsing.Parser
import Text.RawString.QQ
import Interp.Run
import Parsing.AST

parseCheck :: Text -> Pipeline -> IO ()
parseCheck pattern expOutput = do
    case parsePipeline pattern of
      Left err -> expectationFailure $ unpack err
      Right out -> out `shouldBe` expOutput

runOn :: Text -> Text -> Text -> IO ()
runOn pattern inp expOutput =
    case parsePipeline pattern of
      Left err -> expectationFailure $ unpack err
      Right pipe -> do
          out <- runPipeline pipe inp
          out `shouldBe` expOutput


main :: IO ()
main = hspec $ do
    describe "parser" $ do
        describe "commands" $ do
            specify "~" $ do
                parseCheck "~ 'a'" $ re' "a"
            specify "+~" $ do
                parseCheck "+~ 'a'" $ addRe' "a"
            specify "-~" $ do
                parseCheck " -~ 'a'" $ removeRe' "a"
            specify "!" $ do
                parseCheck "! tr a-z A-Z" $ sh' "tr" ["a-z", "A-Z"]
            specify "!{}" $ do
                parseCheck "!{echo a?b d }" $ shSub' "echo" [[ Right "a", Left (), Right "b" ], [Right "d"]]
            specify "^{}" $ do
                parseCheck "^{ ~ 'a' }" $ map' (re' "a")
                parseCheck "^{ ~ 'a' | ^{ ~ 'blah' } }" $ map' (re' "a" >> map' (re' "blah"))
            specify "%{}" $ do
                parseCheck "%{ ~ 'a' }" $ each' (re' "a")
            specify ">" $ do
                parseCheck "~ 'a' | >" $ re' "a" >> filter'
            -- xspecify "?!" $ do
                -- parseCheck "" [ShSub "echo" [[ Right "a", Left (), Right "b" ], [Right "d"]]]
            -- xspecify "?{}" $ do
            --     parseCheck "~ '\\w+' | ?~ 'a' " $ map' (re' "a" >> map' (re' "blah"))
            -- xspecify "?{}" $ do
            --     parseCheck "^(~ 'a' )" [Map [re' "a"]]
        describe "pipelines" $ do
            it "should parse pipelines" $ do
                parseCheck "~ 'a' | ~ 'b'" $ re' "a" >> re' "b"
            it "should handle escaped quotes" $ do
                parseCheck [r|~ 'a\'' | ~ "b\"" |] $ re' "a\'" >> re' "b\""
    describe "run" $ do
        describe "commands" $ do
            specify "~" $ do
                "~ 'a' | ! tr a-z A-Z" `runOn` "bab" $ "bAb"
                "~ '\\w+' | ~ '^..' | ! tr a-z A-Z" `runOn` "abcde tuvz" $ "ABcde TUvz"
            specify "!" $ do
                "!{echo -n a?b d }" `runOn` "thing" $ "athingb d"
            specify "!{}" $ do
                "!{echo -n a?b d }" `runOn` "thing" $ "athingb d"
            specify "^{}" $ do
                "~ 'a\\w+' | ^{ ~ '.$' | ! tr 'a-z' 'A-Z' } | ! rev | ! tr -d \\n" `runOn` "abc defgh ape" $ "Cba defgh Epa"
            specify "%{}" $ do
                "~ '\\w+' | %{ ! sort }" `runOn` "[how you doin]" $ "[doin how you]"
            specify ">" $ do
                "~ 'a\\w+' | > " `runOn` "violets are blue" $ "are"
            specify "+~" $ do
                "~ 'banana' | +~ 'apple' | > " `runOn` "apple banana carrot" $ "applebanana"
            specify "~-" $ do
                "~ 'banana' | -~ 'na' | > " `runOn` "apple banana carrot" $ "ba"
