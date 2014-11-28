module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault)
where

import Control.Applicative

sumOfMultiplesDefault :: Int -> Int
sumOfMultiplesDefault = sumOfMultiples [3,5]

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples list n = sum $ filter combinedPredicate [1..n-1]
    where combinedPredicate x = any (x `isDivisibleBy`) list
          x `isDivisibleBy` y = x `mod` y == 0
