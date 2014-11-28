module LeapYear(isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year =
    divisibleBy 4 &&
        not (divisibleBy 100) || divisibleBy 400
    where divisibleBy n = year `mod` n == 0
