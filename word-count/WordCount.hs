module WordCount (wordCount)
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List(foldl')
import Data.Char(isAlphaNum)

wordCount :: String -> M.Map String Int
wordCount = foldl' (\dict word -> M.insertWith (+) (T.unpack $ T.toCaseFold word) 1 dict) M.empty
    . filter (/= T.empty) . T.split (not . isAlphaNum) . T.pack

