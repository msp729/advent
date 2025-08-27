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
    F10.calcDay 7 $
        lines
            "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20"
    return ()

runDay :: Int -> IO ()
runDay n
    | 1 <= n && n <= 5 = F5.calcDay n =<< readDay n
    | 6 <= n && n <= 10 = F10.calcDay n =<< readDay n

readDay :: Int -> IO [String]
readDay n = lines <$> readFile ("./inputs/" ++ show n ++ ".txt")
