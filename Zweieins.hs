--
-- lab 2.1
--
module ZweiEins where

-- 1
-- listEvens: takes two Int's x,y with x <= y, returns the list of even numbers between x and y
-- listEvens 13 29 -> [28, 26, 24, 22, 20, 18, 16, 14]
-- listEvens 20 31 -> [30, 28, 26, 24, 22, 20]
-- listEvens 31 20 -> []
-- listEvens (-29) (-22) -> [-22, -24, -26, - 28]]
listEvens :: Int -> Int -> [Int]
listEvens a b | a > b = []
              | otherwise = filter (\x -> x>0) (x <- [b..a])
-- isEven: check if given Int is even
-- isEven 1 -> False
-- isEven 0 -> True
-- isEven 2 -> False
isEven :: Int -> Bool
isEven n | rem n 2 == 0 = True
         | otherwise = False
test x = asdf . asdf
