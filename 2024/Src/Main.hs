module Src.Main (main) where

import Control.Spoon (spoon)
import qualified Src.F10 as F10
import qualified Src.F5 as F5
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- map (spoon . read) <$> getArgs
    let
        nums, days :: [Int]
        nums = args >>= maybe [] pure
        days = filter (liftA2 (&&) (<= 25) (>= 1)) nums
    mapM_ runDay days
    return ()

runDay :: Int -> IO ()
runDay n = do
    calcDay n =<< readSample n
    putStrLn ""
    calcDay n =<< readDay n

calcDay :: Int -> [String] -> IO ()
calcDay n
    | 1 <= n && n <= 5 = F5.calcDay n
    | 6 <= n && n <= 10 = F10.calcDay n
    | otherwise = undefined

readDay, readSample :: Int -> IO [String]
readDay n = lines <$> readFile ("./inputs/" ++ show n ++ ".txt")
readSample n = lines <$> readFile ("./samples/" ++ show n ++ ".txt")
