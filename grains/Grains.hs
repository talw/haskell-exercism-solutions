module Grains(square, total)
where

-- Optimized for readability
square :: Int -> Integer
{-square 1 = 1-}
{-square n = 2 * square (n - 1)-}
square n = 2 ^ (n - 1)

total :: Integer
{-total = sum [square n | n <- [1..64]]-}
total = square 65 - 1

-- Optimized for Speed (Or at least, that's my guess! If scanl uses the previous calculation
-- for the next)
total2 :: Integer
total2 = square 65 - 1

square2 :: Int -> Integer
square2 n = scanl (*) 1 (repeat 2) !! (n - 1)


