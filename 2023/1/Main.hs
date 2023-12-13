{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Char (isDigit)
import Prelude hiding (readFile, lines, filter, head, last)
import Data.Text (Text, lines, replace, filter, head, last)
import Data.Text.IO

main :: IO ()
main = do
    ftext <- readFile "./input" -- read the file
    let flines = lines ftext -- split it out into lines
    let processed = line <$> flines -- process the lines
    print $ sum processed -- print their sum
    return () -- :)


line :: Text -> Int
line = read . combine . gettwo . linef . wordr
    where
    wordr :: Text -> Text
    wordr = foldr ((.) . uncurry replace) id $ reverse
        [
            ("nine", "n9ne"),
            ("eight", "ei8ht"),
            ("seven", "se7en"),
            ("six", "s6x"),
            ("five", "fi5e"),
            ("four", "fo4r"),
            ("three", "th3ee"),
            ("two", "t2o"),
            ("one", "o1e")
        ]

    linef :: Text -> Text
    linef = filter isDigit -- ignore non-digits

    gettwo :: Text -> (Char, Char)
    gettwo s = (head s, last s) -- get the first and last digit

    combine :: (Char, Char) -> String
    combine (a, b) = [a, b] -- turn them into a string: ('a', 'b') -> "ab"
