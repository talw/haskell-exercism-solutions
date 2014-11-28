module Roman (numerals)
where

import Data.Map (findWithDefault, keys, Map, fromList,
  empty, insert, union, foldrWithKey)

valMapRaw :: Map Int Char
valMapRaw = fromList [(1, 'I'), (5, 'V'),
                  (10, 'X'), (50, 'L'),
                  (100, 'C'), (500, 'D'),
                  (1000, 'M')]

valMap :: Map Int String
valMap = foldrWithKey func empty valMapRaw
  where func n c
          | n == 1 || head (show n) == '5' = insert n [c]
          | otherwise = union $ fromList [(n, [c]), extraElem1, extraElem2]
          where extraElem1 = letterWithPrevious n $ n `div` 10
                extraElem2 = letterWithPrevious (n `div` 2) $ n `div` 10
                letterWithPrevious x prevX =
                  (x - prevX, find prevX valMapRaw : [find x valMapRaw])

numerals :: Int -> String
numerals = numerals' $ reverse $ keys valMap
  where numerals' _ 0 = []
        numerals' descVals n = biggestLetter ++
          numerals' descVals' (n - biggestVal)
          where biggestLetter = find biggestVal valMap
                biggestVal = head descVals'
                descVals' = dropWhile (n <) descVals

find :: Ord k => k -> Map k a -> a
find = findWithDefault (error "bug")
