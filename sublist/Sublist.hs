module Sublist (Sublist(..), sublist)
where

import Data.List (isInfixOf)

data Sublist = Equal | Sublist | Superlist | Unequal
    deriving (Show, Eq)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist a b = case (a `isInfixOf` b, b `isInfixOf` a) of
                (True, False) -> Sublist
                (True, True) -> Equal
                (False, True) -> Superlist
                (False, False) -> Unequal
