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
              | otherwise = listEvensHelp (reverseList [a..b])
listEvensHelp xs = filter (\x -> isEven x) xs
-- isEven: check if given Int is even
-- isEven 1 -> False
-- isEven 0 -> True
-- isEven 2 -> False
isEven :: Int -> Bool
isEven n | rem n 2 == 0 = True
         | otherwise = False
--reverseList: for given xs return reversed list
--reverseList [1,2,3] -> [3,2,1]
reverseList :: [Int] -> [Int]
reverseList xs = reverseListHelp xs []
reverseListHelp :: [Int] -> [Int] -> [Int]
reverseListHelp [] xs = xs
reverseListHelp xs ys = reverseListHelp (tail xs) ((head xs):ys)

-- 2
-- listTables: take Int n>0 and return triplets with a,b,c>=n and c=a*b
-- use:
type Table = (Int, Int, Int)
listTables :: Int -> [Table]
listTables n | n<0 = []
             | otherwise = [(a, b, c) | a<-[0..n], b<-[0..n], c<-[0..n], c==a*b ]
