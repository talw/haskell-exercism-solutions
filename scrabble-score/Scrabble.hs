module Scrabble (scoreLetter, scoreWord)
where

import Data.Map (Map, insert, lookup, empty)
import Prelude hiding (lookup)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)

scoreWord :: String -> Int
scoreWord = foldl' (+) 0 . map scoreLetter

scoreLetter :: Char -> Int
scoreLetter c = fromMaybe (error "invalid character") $ lookup (toUpper c) scoreMap

scoreMap :: Map Char Int
scoreMap = foldl' accGradeLetters empty scoreList
    where scoreList = [--Would have been configurable if it was a real game
            ("AEIOULNRST",1),
            ("DG",2),
            ("BCMP",3),
            ("FHVWY",4),
            ("K",5),
            ("JX",8),
            ("QZ",10) ]
          accGradeLetters dict pair =
              foldl' (\innerDict c -> insert c (snd pair) innerDict) dict $ fst pair

