module Binary (toDecimal)
where

import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (liftM2)

toDecimal :: String -> Int
toDecimal str
    | any (liftM2 (&&) (/= '0') (/= '1')) str = 0
    | otherwise = foldl' (\acc x -> 2 * acc + x) 0 $ map digitToInt str
