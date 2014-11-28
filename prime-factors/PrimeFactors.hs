module PrimeFactors (primeFactors)
where

primeFactors :: Int -> [Int]
primeFactors = primeFactors' [2..]
  where primeFactors' _ 1 = []
        primeFactors' candidatesLeft n = nextDivider : primeFactors' candidatesLeft' n'
          where n' = n `div` nextDivider
                nextDivider = head candidatesLeft'
                candidatesLeft' = dropWhile (not . (n `divisibleBy`)) candidatesLeft

divisibleBy :: Int -> Int -> Bool
divisibleBy = ((== 0) .) . mod

{-sieve :: [Int]-}
{-sieve = sieve' [2..]-}
  {-where sieve' list =-}
          {-let currNum = head list-}
              {-rest = filter (not . (`divisibleBy` currNum)) $ tail list-}
          {-in currNum : sieve' rest-}
