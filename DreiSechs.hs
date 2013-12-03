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
traversal :: Tree a -> [a]
traversal Empty = []
traversal (Leaf x) = [x]
traversal(Node x l r) = traversal l ++ [x] ++ traversal r

post :: Tree a -> [a]
post (Leaf x) = [x]
post (Node x a b) = post a ++ post b ++ [x]

pre :: Tree a -> [a]
pre (Leaf x) = [x]
pre (Node x a b) = [x] ++ pre a ++ pre b
                   
-- 5
normalise :: Tree a -> Tree a
normalise (Node x Empty Empty) = Leaf x
normalise (Node x a b) = Node x (normalise a) (normalise b)
normalise a = a

-- 6
natural :: Int -> Bool
natural n = n >= 0
plusOne :: Int -> Int
plusOne x = x + 1

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node x a b) = Node (f x) (mapTree f a) (mapTree f b)

-- 7
-- TODO
-- fix this faulty implementation
lists2tree :: Eq a => [a] -> [a] -> Tree a
lists2tree [] _ = Empty
lists2tree _ [] = Empty
                 -- xs traversal
                 -- ys pre
lists2tree xs ys = Node (head xs) (lists2tree (tail xs) ys) (lists2tree (tail (tail xs)) ys)

-- chkk (lists2tree [1,2,3,4,5] [3,2,4,1,5]) Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)
-- traversal(Node x l r) = traversal l ++ [x] ++ traversal r
-- pre (Node x a b) = [x] ++ pre a ++ pre b
