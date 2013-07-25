--
-- lab 1.5
--
module EinsFuenf where

{-type Moves = (Char, Char)-}
{-type Solution = [Moves]-}

-- hanoi: towers of hanoi
-- hanoi 0 -> []
-- hanoi 2 -> [('a', 'b'), ('a', 'c'), ('b', 'c')]
{-hanoi :: Int -> Solution-}
{-hanoi 0 = []-}
{-hanoi n = compute (n, 'a', 'b', 'c')-}

{-compute :: Int -> Char -> Char -> Char -> Solution-}
{-compute n a b c = compute(n - 1) a b c ++ (a, c) ++ compute(n - 1) b c a-}
