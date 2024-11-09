module Main (main) where

import Data.Char

main :: IO ()
main = do
    ftext <- readFile "./input"
    print $ sum $ map (number . numberify) $ lines ftext
    return ()

prefix :: String -> String -> Bool
prefix (a : as) (b : bs) = a == b && prefix as bs
prefix [] _ = True
prefix _ [] = False

numberify :: String -> String
numberify [] = []
numberify xs
    | prefix "zero" xs = '0' : numberify (drop 1 xs)
    | prefix "one" xs = '1' : numberify (drop 1 xs)
    | prefix "two" xs = '2' : numberify (drop 1 xs)
    | prefix "three" xs = '3' : numberify (drop 1 xs)
    | prefix "four" xs = '4' : numberify (drop 1 xs)
    | prefix "five" xs = '5' : numberify (drop 1 xs)
    | prefix "six" xs = '6' : numberify (drop 1 xs)
    | prefix "seven" xs = '7' : numberify (drop 1 xs)
    | prefix "eight" xs = '8' : numberify (drop 1 xs)
    | prefix "nine" xs = '9' : numberify (drop 1 xs)
    | otherwise = head xs : numberify (tail xs)

number :: String -> Int
number s = 10 * a + b
  where
    s' = filter isDigit s
    a = read $ pure $ head s'
    b = read $ pure $ last s'
