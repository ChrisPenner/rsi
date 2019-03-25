{-# LANGUAGE ViewPatterns #-}
module Lib where

-- import Text.Regex.TDFA.String
-- import Text.Regex.TDFA
import Text.Regex.PCRE
import Data.String as T
import Data.BitVector as BV
import Data.List as L

getMatches :: String -> String -> BV
getMatches re txt =
    let AllMatches matches = txt =~ re :: AllMatches [] (MatchOffset, MatchLength)
    in BV.or $ zeros (length txt) : fmap mkBitV matches
  where
    pad :: BV -> BV
    pad bv = bv # zeros (L.length txt - BV.size bv)

    mkBitV :: (MatchOffset, MatchLength) -> BV
    mkBitV (offset, len) = pad (BV.zeros offset BV.# BV.ones len)

showMatchRanges :: String -> String -> [(Int, Int)]
showMatchRanges re txt =
    let AllMatches matches = txt =~ re :: AllMatches [] (MatchOffset, MatchLength)
    in matches

showMatches :: String -> String -> [String]
showMatches re txt =
    let AllTextMatches matches = txt =~ re :: AllTextMatches [] String
    in matches

showBV :: BV -> String
showBV bv = foldMap s $ BV.toBits bv
  where
    s True  = "1"
    s False = "0"

printSelection :: String -> String -> IO ()
printSelection re txt =
    let mask = getMatches re txt
    in do
        putStrLn txt
        putStrLn $ showBV mask

printSelectionS :: [String] -> String -> IO ()
printSelectionS res txt = do
    let mask = stackRegexes res txt
    putStrLn txt
    putStrLn $ showBV mask

runMatch :: String -> BV -> String -> BV
runMatch txt bv re = concatBits (matchEach <$> groupUp txt bitList)
    where
     concatBits = foldl' (#) (fromBits [])
     bitList =  toBits bv
     matchEach (True, s) = getMatches re s
     matchEach (False, s) = zeros (length s)

stackRegexes :: [String] -> String -> BV
stackRegexes res txt = L.foldl' (runMatch txt) (ones (L.length txt)) res

groupUp :: String -> [Bool] -> [(Bool, String)]
groupUp [] [] = []
groupUp cs bools@(b: _) = (b, current) : groupUp rString rBools
    where
        (current, rString) = L.splitAt ln cs
        (length -> ln, rBools) = span (==b) bools

