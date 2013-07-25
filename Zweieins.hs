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

-- 3
-- addPairwise: take xs,ys and add corresponding elements
-- addPairwise [1, 2] [3, 4, 5] -> [4, 6]
-- addPairwise [] [1, 2, 3] -> []
-- addPairwise [(-7), (-8), (-9)] [10, 11, 12] -> [3, 3, 3]
addPairwise :: [Int] -> [Int] -> [Int]
addPairwise [] _ = []
addPairwise _ [] = []
addPairwise xs ys = [x + y | (x, y) <- zip xs ys]

-- 4
-- subList: take list xs and indices (i,j) return list [xs_i .. xs_j] for 0<=i<=j<=length(xs-1)
-- subList [0, 1, 2, 3, 4, 5] (2, 4) -> [2, 3, 4]
-- subList ['a', 'b', 'c', 'd', 'e'] (1, 3) -> ['b', 'c', 'd']
subList :: [a] -> (Int, Int) -> [a]
subList xs (i, j) | j < i = error "i > j, cannot be..."
                  | i >= length xs || i <= 0 = error "ArrayIndex i out of bounds."
                  | j >= length xs || i <= 0 = error "Arrayjndex j out of bounds."
                  | i == j = xs !! i:[]
                  | otherwise = [ xs!!k | k<-[i..j] ]
