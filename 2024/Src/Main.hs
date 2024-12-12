module Src.Main (main) where

import Control.Spoon (spoon)
import qualified Src.F5 as F5
import qualified Src.F10 as F10
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
runDay n
    | 1 <= n && n <= 5 = F5.calcDay n =<< readDay n

readDay :: Int -> IO [String]
readDay n = lines <$> readFile ("./inputs/" ++ show n ++ ".txt")
