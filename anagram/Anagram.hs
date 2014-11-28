module Anagram (anagramsFor)
where

import Data.List
import Data.Char
import Control.Monad

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter $ liftM2 (&&) isPermutationof (map toLower word /=) . map toLower
    where isPermutationof otherWord = map toLower word `isPermutationof'` otherWord

          [] `isPermutationof'` [] = True
          [] `isPermutationof'` (_:_) = False
          (x:xs) `isPermutationof'` ys
              | x `elem` ys = isPermutationof' xs $ delete x ys
              | otherwise = False
