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
