module School(sorted, empty, grade, add, School)
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L

newtype School = School { unSchool :: M.Map Int [String] }

empty :: School
empty = School M.empty

add :: Int -> String -> School -> School
add g name (School s) = School $ M.insertWith insertFunc g [name] s
    where insertFunc = L.insert . head

sorted :: School -> [(Int, [String])]
sorted = M.toList . unSchool

grade :: Int -> School -> [String]
grade g = fromMaybe [] . M.lookup g . unSchool
