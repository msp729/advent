module Src.F5 (calcDay) where

calcDay :: Int -> [String] -> IO ()
calcDay = ([day1, day2, day3, day4, day5] !!)

day1, day2, day3, day4, day5 :: [String] -> IO ()
day1 ls = do
    let
        pairs :: [(Int, Int)]
        pairs = map (liftA2 (,) (!! 0) (!! 1) . map read . words) ls
    print $ sum $ map (abs . uncurry (-)) pairs
day2 = undefined
day3 = undefined
day4 = undefined
day5 = undefined
