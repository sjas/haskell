--
-- lab 3.6
--
module DreiSechs where
    
data Tree a = Empty 
	    | Leaf a 
	    | Node a (Tree a) (Tree a) 
            deriving (Eq,Show)

-- 1
howMany :: Tree a -> Integer
howMany Empty = 0
howMany (Leaf _) = 1
howMany (Node _ a b) = 1 + howMany a + howMany b

-- 2
depth :: Tree a -> Integer
depth Empty = 0
depth (Leaf _) = 1
depth (Node _ a b) = 1 + max (depth a) (depth b)
                     
-- 3
reflect :: Tree a -> Tree a
reflect Empty = Empty
reflect (Leaf a) = Leaf a
reflect (Node x a b) = Node x (reflect b) (reflect a)

-- 4
pre :: Tree a -> [a]
pre (Leaf x) = [x]
pre (Node x a b) = [x] ++ pre a ++ pre b
traversal :: Tree a -> [a]
traversal Empty = []
traversal (Leaf x) = [x]
traversal(Node x l r) = traversal l ++ [x] ++ traversal r
post :: Tree a -> [a]
post (Leaf x) = [x]
post (Node x a b) = post a ++ post b ++ [x]
                   
-- 5
normalise :: Tree a -> Tree a
normalise (Node x Empty Empty) = Leaf x
normalise (Node x a b) = Node x (normalise a) (normalise b)
normalise a = a

-- 6
natural :: Integer -> Bool
natural n = n >= 0
plusOne :: Integer -> Integer
plusOne x = x + 1
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node x a b) = Node (f x) (mapTree f a) (mapTree f b)

-- 7
lists2tree :: Eq a => [a] -> [a] -> Tree a
lists2tree [] _ = Empty
lists2tree _ [] = Empty
lists2tree (x:[]) (y:[]) | x == y = Leaf x
                         | otherwise = Empty
-- xs is traversal, ys is pre
lists2tree (x:xs) ys = Node x (lists2tree (listDiff xs sh) fh) (lists2tree (listDiff xs fh) sh)
                       where 
                         fh = firstHalf x ys
                         sh = secondHalf x ys
firstHalf :: Eq a => a -> [a] -> [a]
firstHalf _ [] = []
firstHalf n (x:xs) | n == x = []
                   | otherwise = x : firstHalf n xs
secondHalf :: Eq a => a -> [a] -> [a]
secondHalf n (x:xs) | n == x = xs
                    | otherwise = secondHalf n xs
listDiff :: Eq a => [a] -> [a] -> [a]
listDiff [] _ = []
listDiff xs [] = xs
listDiff xs (y:ys) | y `elem` xs = listDiff (remElem y xs) ys
                   | otherwise = listDiff xs ys
remElem :: Eq a => a -> [a] -> [a]
remElem x ys = [ y | y <- ys, x /= y ]
