{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsing.Parser where

import Prelude hiding (lex)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Char
import Data.Text as T
import Data.Void
import Operators.Combinators
import Parsing.AST
import Data.Bifunctor
import Control.Monad
import Data.List as L
import Text.RawString.QQ

type Parser a = Parsec Void Text a

sym :: Text -> Parser Text
sym = symbol space

lex :: Parser a -> Parser a
lex = lexeme space

quoted' :: Char -> Parser Text
quoted' delimeter = lex $ pack <$> between (single delimeter) (single delimeter) (many (try escaped <|> normalChar))
   where
     escaped = single '\\' *> single delimeter
     normalChar = anySingleBut delimeter

singleQuoted, doubleQuoted, quoted :: Parser Text
singleQuoted = quoted' '\''
doubleQuoted = quoted' '\"'
quoted = singleQuoted <|> doubleQuoted

parsePipeline :: Text -> Either Text [Pipeline]
parsePipeline pipelineText = first (T.pack . errorBundlePretty) $ parse pipeline "" pipelineText

reP :: Parser Pipeline
reP = do
    sym "~"
    pattern <- quoted
    return (Re pattern)

shP :: Parser Pipeline
shP = do
    sym "!"
    cmd <- word
    args <- many (lex arg)
    space
    return (Sh cmd args)

shSubP :: Parser Pipeline
shSubP = do
    sym "!"
    between (sym "{") (sym "}") $ do
        cmd <- word
        args <- many (lex arg)
        return (ShSub cmd (findSub <$> args))
  where
    findSub :: Text -> [Either () Text]
    findSub = L.intersperse (Left ()) . fmap Right . T.splitOn "?"

mapP :: Parser Pipeline
mapP = do
    sym "%"
    Map <$> between (sym "{") (sym "}") pipeline

arg :: Parser Text
arg = quoted <|> word

word :: Parser Text
word = lex $ pack <$> some (noneOf (" \n\t{}|" :: [Char]))

op :: Parser Pipeline
op = choice [ reP, try shSubP, shP, mapP ]

pipeline :: Parser [Pipeline]
pipeline = (op `sepBy1` sym "|")

ast :: Parser [Pipeline]
ast = pipeline <* eof

-- test = parseMaybe
