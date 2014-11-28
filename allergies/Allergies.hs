module Allergies (Allergen(..), isAllergicTo, allergies)
where

import Data.Bits (testBit)

data Allergen = Eggs |
                Peanuts |
                Shellfish |
                Strawberries |
                Tomatoes |
                Chocolate |
                Pollen |
                Cats
        deriving (Ord, Eq, Show, Enum)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a i = testBit i $ fromEnum a

allergies :: Int -> [Allergen]
allergies n = filter (`isAllergicTo` n) $ map toEnum [0..7]

