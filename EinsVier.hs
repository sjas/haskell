--
-- lab 1.4
--
module EinsVier where

{-type Moves = (Char, Char)-}
{-type Solution = [Moves]-}

-- hanoi: towers of hanoi
-- hanoi 0 -> []
-- hanoi 2 -> [('a', 'b'), ('a', 'c'), ('b', 'c')]


-- Aguado implmentation

type Setting = (Char, Char, Char)
-- origin,intermediate,destination are tower names: o,i,d

{-hanoi :: Setting -> Int -> String-}
hanoi _ 0 = []
{-hanoi (o,i,d) 1 = (o,d)-}
hanoi (o,i,d) n = hanoi (o,d,i) (n-1) ++ [(o,d)] ++ hanoi (d,i,o) (n-1)


-- other implementation
{-
hanoi :: Int -> Solution
hanoi 0 = []
hanoi n = compute (n, 'a', 'b', 'c')

compute :: Int -> Char -> Char -> Char -> Solution
compute n a b c = compute(n - 1) a b c ++ (a, c) ++ compute(n - 1) b c a
-}
