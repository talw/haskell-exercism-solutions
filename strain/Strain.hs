module Strain (keep, discard)
where

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr addIfPred []
    where addIfPred x xs = if p x
                            then x:xs
                            else xs

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep $ not . p
