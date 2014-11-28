module Phone (areaCode, number, prettyPrint)
where

import Data.Char


emptyNumber :: String
emptyNumber = "0000000000"


breakNumber :: String -> (String, String, String)
breakNumber num = (ac, mid, end)
    where (ac, rest) = splitAt 3 $ number num
          (mid, end) = splitAt 3 rest

number :: String -> String
number num = case length numD of
                10 -> numD
                11 | head numD == '1' -> tail numD
                otherwise -> emptyNumber
    where numD = digits num
          digits = filter isDigit

areaCode :: String -> String
areaCode num = let (ac, _, _) = breakNumber num
                in ac

prettyPrint :: String -> String
prettyPrint num = "(" ++ ac ++ ")" ++ " " ++ mid ++ "-" ++ end
    where (ac, mid, end) = breakNumber num



