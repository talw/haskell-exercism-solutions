module Raindrops (convert)
where

convert :: Int -> String
convert x
  | null sentence = show x
  | otherwise = sentence
  where sentence = concat [func 3 "Pling", func 5 "Plang", func 7 "Plong"]
        func n str
          | x `mod` n == 0 = str
          | otherwise = ""
