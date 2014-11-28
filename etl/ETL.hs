module ETL (transform)
where

import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Char (toLower)

transform :: M.Map Int [String] -> M.Map String Int
transform = M.foldrWithKey' addPointLetters M.empty
    where addPointLetters n cs acc = foldl' (\acc' c -> M.insert c n acc') acc cs'
              where cs' = map (map toLower) cs
