module DNA (hammingDistance)
where

hammingDistance :: String -> String -> Int
hammingDistance s1 s2 = sum $ zipWith (\x y -> if x /= y then 1 else 0) s1 s2
