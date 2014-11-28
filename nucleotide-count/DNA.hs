module DNA (count, nucleotideCounts)
where

import qualified Data.Map as M

type Nucleotide = Char
type Strand = String

nucleotides :: String
nucleotides = "ATCG"

count :: Nucleotide -> Strand -> Int
count nuc
    | nuc `elem` nucleotides = length . filter (nuc ==)
    | otherwise = error "invalid nucleotide 'X'"

for = flip map

nucleotideCounts :: Strand -> M.Map Nucleotide Int
nucleotideCounts str = M.fromList $ for nucleotides $ \x -> (x, count x str)

