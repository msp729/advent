{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE Strict #-}

module Src.F10 (calcDay) where

import Control.Applicative (liftA3)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S

calcDay :: Int -> [String] -> IO ()
calcDay 6 = day6
calcDay 7 = day7
calcDay 8 = day8
calcDay 9 = day9
calcDay 10 = day10

day6, day7, day8, day9, day10 :: [String] -> IO ()

data Direction = N | S | E | W deriving (Show, Eq, Enum, Ord)

-- | day 6
day6 grid = do
    print $ length guardPath
    print $ length loopObstacles
  where
    h = length grid
    w = length $ head grid
    get (y, x) = grid !! y !! x
    locations = [(y, x) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
    obstacles = filter ((== '#') . get) locations
    Just iy = findIndex (elem '^') grid
    Just ix = elemIndex '^' $ grid !! iy

    move :: Direction -> (Int, Int) -> (Int, Int)
    move N (y, x) = (y - 1, x)
    move S (y, x) = (y + 1, x)
    move E (y, x) = (y, x + 1)
    move W (y, x) = (y, x - 1)

    turn :: Direction -> Direction
    turn N = E
    turn E = S
    turn S = W
    turn W = N

    update :: (Direction, (Int, Int)) -> (Direction, (Int, Int))
    update (d, p) = let p' = move d p in if elem p' obstacles then (turn d, p) else (d, p')
    inGrid (d, (y, x)) = y >= 0 && x >= 0 && y < h && x < w
    guardPath :: [(Int, Int)]
    guardPath = nub $ snd <$> takeWhile inGrid path'
      where
        path' = iterate update (N, (iy, ix))

    possibleObstacles :: [(Int, Int)]
    possibleObstacles = drop 1 guardPath

    update' :: (Int, Int) -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
    update' ob (d, p) = let p' = move d p in if elem p' obstacles || p' == ob then (turn d, p) else (d, p')
    prospect :: (Int, Int) -> [(Direction, (Int, Int))]
    prospect ob = iterate (update' ob) (N, (iy, ix))
    -- returns one of the following, whichever is shorter:
    --  - the longest prefix of the given list with no duplicates (longest prefix to a loop)
    --  - the longest prefix of the given list s.t. all values evaluate to True under the predicate
    -- the attached Bool is True if it's a loop prefix, False if it's a takeWhile
    getPrefix :: (Eq a, Ord a) => (a -> Bool) -> [a] -> (Bool, [a])
    getPrefix p = fmap reverse . helper p mempty
      where
        helper :: (Eq a, Ord a) => (a -> Bool) -> Set a -> [a] -> (Bool, [a])
        helper p s [] = (False, [])
        helper p s (x : xs)
            | x `S.member` s = (True, [])
            | p x = (x :) <$> helper p (S.insert x s) xs
            | otherwise = (False, [])

    loopObstacles = filter (fst . getPrefix inGrid . prospect) possibleObstacles

type I7 = Int
type S7 = IntSet

-- | day 7
day7 txt = do
    print part1
    print part2
  where
    parser :: String -> (I7, I7, [I7])
    parser s =
        let
            (stv : sfv : srest) = words s
            tv, fv :: I7
            tv = read $ init stv
            fv = read sfv
            rest :: [I7]
            rest = read <$> srest
         in
            (tv, fv, rest)

    combs :: S7 -> I7 -> S7
    combs s x = liftA2 IS.union (IS.map (x *)) (IS.map (x +)) s

    combs' :: I7 -> S7 -> I7 -> S7
    combs' t s x = liftA3 ((IS.union .) . IS.union) (IS.map (x *)) (IS.map (x +)) (IS.map (>< x)) s

    attemptline, attempt' :: (I7, I7, [I7]) -> Maybe I7
    attemptline (tv, fv, rv) = if IS.member tv $ foldl' combs (IS.singleton fv) rv then Just tv else Nothing
    attempt' (tv, fv, rv) = if IS.member tv $ foldl' (combs' tv) (IS.singleton fv) rv then Just tv else Nothing

    (><) :: I7 -> I7 -> I7
    a >< b = read (show a <> show b)

    parsed = parser <$> txt
    part1 = sum $ catMaybes $ attemptline <$> parsed
    part2 = sum $ catMaybes $ attempt' <$> parsed

-- | day 8
day8 = undefined

-- | day 9
day9 = undefined

-- | day 10
day10 = undefined
