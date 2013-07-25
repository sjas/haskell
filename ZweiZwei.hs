--
-- lab 2.2
--
module ZweiZwei where

-- 1
-- theElem: True if elem is present in xs
-- theElem [2..22] 17 -> True
-- theElem [2..22] 1 -> False
theElem :: Eq a => [a] -> a -> Bool
theElem [] _ = False
theElem xs x = if (x /= head xs)
                then theElem (tail xs) x
                else True

-- 2
-- theLast: return last element of a non-empty list
-- theLast [1..3] -> 3
theLast :: Eq a => [a] -> a
theLast xs = if (tail xs == [])
                 then head xs
                 else theLast (tail xs)

-- 3
-- theLenght: return length of a list
-- thelenght [1..100] -> 100
theLenght :: Ord a => [a] -> Int
theLenght xs = if (tail xs == [])
               then 1
               else 1 + (theLenght (tail xs))
-- 4
--
