module Bob (responseFor)
where

import Data.Char

responseFor :: String -> String
responseFor str
    | all isSpace str = "Fine. Be that way!"
    -- The reason for allButAtLeastOne is because otherwise, an input like "123?" would get
    -- "Whoa, chill out!" but that's a calm question
    | allButAtLeastOne isUpper $ filter isAlpha str = "Whoa, chill out!"
    | last str == '?' = "Sure."
    | otherwise = "Whatever."
    where allButAtLeastOne _ [] = False
          allButAtLeastOne p list = all p list
