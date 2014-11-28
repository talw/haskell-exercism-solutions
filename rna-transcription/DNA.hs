module DNA (toRNA) where

newtype Nucleotide = N { unN :: Char }
    deriving Show

toN :: Char -> Nucleotide
toN c = case c of
     G -> N 'G'
     C -> N 'C'
     T -> N 'T'
     A -> N 'A'
     U -> N 'U'
     otherwise -> error "invalid nucleotide letter"

type Strand = [Nucleotide]
{-newtype Strand = Strand [Nucleotide]-}
{-nucs :: Strand -> [Nucleotide]-}
{-nucs (Strand nucs) = nucs-}

{-instance Show Strand where-}
    {-show (Strand nucs) = map (head . show) nucs-}

toRNA :: String -> String
toRNA = show . toRNA' . map toN

toRNA' :: Strand -> Strand
toRNA' = map convertRNAToDNA
    where convertRNAToDNA nuc = toN $ case nuc of
                                             (toN 'G') -> 'C'
                                             (toN 'C') -> 'G'
                                             (toN 'T') -> 'A'
                                             (toN 'A') -> 'U'
