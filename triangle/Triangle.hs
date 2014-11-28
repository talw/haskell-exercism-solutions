module Triangle (TriangleType(..), triangleType)
where

import Control.Monad (replicateM)
import Data.List (sort)

data TriangleType = Equilateral |
                    Isosceles |
                    Scalene |
                    Illogical
                deriving (Show, Eq)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType x y z
    | let [a,b,c] = sort [x,y,z] in c >= a + b = Illogical
    | otherwise = case score of
                        3 -> Scalene
                        5 -> Isosceles
                        9 -> Equilateral
                        _ -> error "Something really bad has happened"
    where score = length $ filter (\[a,b] -> a == b) $ replicateM 2 [x,y,z]
