{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Src.F5 (calcDay) where

import Data.Bifunctor (bimap, first)
import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf, nub, partition, permutations, sort, transpose)
import Data.Maybe (fromJust, fromMaybe)

calcDay :: Int -> [String] -> IO ()
calcDay 1 = day1
calcDay 2 = day2
calcDay 3 = day3
calcDay 4 = day4
calcDay 5 = day5
calcDay n = const $ putStrLn $ "day " ++ show n ++ " not implemented"

{- | Run-length encoding
e.g. [1,1,2,3] -> [(1,2),(2,1),(3,1)]
-}
rle :: (Eq a) => [a] -> [(a, Int)]
rle [] = []
rle (x : xs) = helper 1 x xs
  where
    helper !n x [] = [(x, n)]
    helper !n x (y : ys)
        | x == y = helper (n + 1) x ys
        | otherwise = (x, n) : helper 1 y ys

-- | List differences
delta :: (Num a) => [a] -> [a]
delta [] = []
delta [x] = []
delta (x : y : z) = y - x : delta (y : z)

-- | between l h x is "is l <= x <= h"?
between :: (Ord a) => a -> a -> a -> Bool
between l h = liftA2 (&&) (>= l) (<= h)

lbetween :: Int -> Int -> [a] -> Bool
lbetween l _ [] = l <= 0
lbetween _ 0 _ = True
lbetween l h (_ : xs) = lbetween (l - 1) (h - 1) xs

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile p [] = ([], [])
splitWhile p (x : xs)
    | p x = first (x :) (splitWhile p xs)
    | otherwise = ([], xs)

swap :: Int -> Int -> [a] -> [a]
swap a b l =
    let
        (part1, a' : rest) = splitAt a l
        (part2, b' : rest') = splitAt (b - a - 1) rest
     in
        part1 ++ b' : part2 ++ a' : rest'

day1, day2, day3, day4, day5 :: [String] -> IO ()

{- | day 1, part 1: calculate pairwise diffs between elements of lists, in sorted order.
 part 2: sum over all values in the lists of value * # appearances in left * # appearances in right
-}
day1 ls = do
    let
        pairs' :: [(Int, Int)]
        pairs' = map (liftA2 (,) (!! 0) (!! 1) . map read . words) ls

        lists :: ([Int], [Int])
        lists = bimap sort sort $ foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], []) pairs'

        pairs :: [(Int, Int)]
        pairs = uncurry zip lists

        lists' :: ([(Int, Int)], [(Int, Int)])
        lists' = bimap rle rle lists

        similarity :: Int
        similarity = sum $ map (\(x, l) -> x * l * (fromMaybe 0 (lookup x (snd lists')))) $ fst lists'

    print $ sum $ map (abs . uncurry (-)) pairs
    print $ similarity

{- | day 2, part 1: count the valid reports
 a report is a series of numbers, valid all the the differences have the same sign and are between 1 and 3

 part 2: count valid reports if we now are allowed to ignore one number from the report
-}
day2 rs = do
    let
        reports :: [[Int]]
        reports = map (map read . words) rs

        valid :: [Int] -> Bool
        valid = liftA2 (||) (all (between 1 3) . map negate) (all (between 1 3)) . delta

        damps :: [a] -> [[a]]
        damps [] = []
        damps (x : xs) = xs : map (x :) (damps xs)

        dvalid :: [Int] -> Bool
        dvalid l = valid l || any valid (damps l)
    print $ length $ filter valid reports
    print $ length $ filter dvalid reports

{- | day 3, part 1: find all valid mul statements (described by the regex mul\(\d{1,3},\d{1,3}\))
perform the multiplications and add them.
part 2: the don't() command disables mul(). do() re-enables.

tbh i didn't want to do regex in haskell, so i just did shell commands

cat inputs/3.txt | rg -o 'mul\(\d{1,3},\d{1,3}\)' | sd 'mul\(' '' | sd , ' ' | sd \\\) ' * +' | cat 0 - f | dc
cat inputs/3.txt | rg -o 'mul\(\d{1,3},\d{1,3}\)|do\(\)|don'"'"'t\(\)' > tmp
nvim tmp # use nvim to remove disabled lines by hand
cat tmp | rg -o 'mul\(\d{1,3},\d{1,3}\)' | sd 'mul\(' '' | sd , ' ' | sd \\\) ' * +' | cat 0 - f | dc

the 0 file contained only the letter 0, the f file likewise contained only f.
-}
day3 text = do
    print 183669043
    print 59097164

{- | day 4: count instances of the character sequence XMAS (permissible in all 8 directions)
part 2: count crossed MAS's , e.g.:
M-M
-A-
S-S
-}
day4 text = do
    let
        vtext = transpose text
        h = length text
        w = length (text !! 0)
        diag1 =
            map (\(y, x) -> text !! y !! x)
                <$> [ takeWhile
                        (uncurry (&&) . bimap (< h) (< w))
                        [ (iy + t, ix + t)
                        | t <- [0 ..]
                        ]
                    | iy <- [1 .. h - 1] ++ replicate w 0
                    | ix <- replicate h 0 ++ [1 .. w - 1]
                    ]
        diag2 =
            map (\(y, x) -> text !! y !! x)
                <$> [ takeWhile
                        (uncurry (&&) . bimap (>= 0) (< w))
                        [ (iy - t, ix + t)
                        | t <- [0 ..]
                        ]
                    | iy <- [0 .. h - 2] ++ replicate w (h - 1)
                    | ix <- replicate h 0 ++ [1 .. w - 1]
                    ]
        appearances = helper 0
          where
            helper !n [] = n
            helper !n ('X' : 'M' : 'A' : 'S' : xs) = helper (n + 1) xs
            helper !n (_ : xs) = helper n xs

        locations :: [(Int, Int)]
        locations = [(y, x) | x <- [1 .. w - 2], y <- [1 .. h - 2], text !! y !! x == 'A']

        xmases :: (Int, Int) -> Int
        xmases (y, x) = length $ filter id [diag1 && diag2]
          where
            ismas :: [(Int, Int)] -> Bool
            ismas l = sort "MS" == sort (map (\(dy, dx) -> text !! (y + dy) !! (x + dx)) l)

            diag1, diag2 :: Bool
            diag1 = ismas [(-1, -1), (1, 1)]
            diag2 = ismas [(-1, 1), (1, -1)]
    print $
        sum $
            map appearances $
                [text, vtext, diag1, diag2] >>= \l -> l ++ map reverse l
    print $ sum $ map xmases locations

{- | day 5: rules X|Y specify that X cannot come after Y.
then some lists of numbers are given.

part 1: print the sum of the middle numbers of all the good orders (orders that don't break any rules)

part 2: make the bad ones good, and then print the sum of the middle numbers of the once-bad ones
-}
day5 text = do
    let
        Just (rrules, _ : rorders) = flip splitAt text <$> elemIndex [] text
        rules :: [(Int, Int)]
        rules = nub $ map (bimap read (read . dropWhile (not . isDigit)) . splitWhile isDigit) rrules

        orders :: [[Int]]
        orders = map (read . ('[' :) . (++ "]")) rorders

        follows :: [Int] -> (Int, Int) -> Bool
        follows o (l, h) = fromMaybe True $ liftA2 (<) (elemIndex l o) (elemIndex h o)

        good, bad :: [[Int]]
        (good, bad) = partition (\o -> all (follows o) rules) orders

        makeGood :: [Int] -> [Int]
        makeGood l =
            if all (follows l) relevant
                then l
                else makeGood $ foldr (.) id (map enforce relevant) $ l
          where
            relevant = filter (\(a, b) -> elem a l && elem b l) rules

        enforce :: (Int, Int) -> [Int] -> [Int]
        enforce r@(a, b) = do
            a' <- fromJust <$> elemIndex a
            b' <- fromJust <$> elemIndex b
            if a' > b' then swap b' a' else id
    print $ sum $ map (\l -> l !! div (length l) 2) good
    print $ sum $ map (\l -> makeGood l !! div (length l) 2) bad
