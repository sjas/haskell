--
-- lab 1.4
--
module EinsVier where

type Moves = (Char, Char)
type Solution = [Moves]

-- hanoi: towers of hanoi
-- hanoi 0 -> []
-- hanoi 2 -> [('a', 'b'), ('a', 'c'), ('b', 'c')]
hanoi :: Integer -> Solution
hanoi n = computeHanoi n 'a' 'b' 'c'
computeHanoi :: Integer -> Char -> Char -> Char -> Solution
computeHanoi 0 _ _ _ = []
computeHanoi n a b c = computeHanoi (n-1) a c b ++ [(a, c)] ++ computeHanoi (n-1) b a c

-- Aguado implmentation
-- origin,intermediate,dest are tower names: o,i,d
type Setting = (Char, Char, Char)
ahanoi :: Setting -> Integer -> String
ahanoi _ 0 = []
ahanoi (o,_,d) 1 = o : [d]
ahanoi (o,i,d) n = ahanoi (o,d,i) (n-1) ++ [o] ++ [d] ++ ahanoi (i,o,d) (n-1)
