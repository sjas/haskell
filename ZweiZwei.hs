--
-- lab 2.2
--
module ZweiZwei where

-- 1
-- theElem: True if elem is present in xs
-- use if-then-else
-- theElem [2..22] 17 -> True
-- theElem [2..22] 1 -> False
theElem :: Eq a => [a] -> a -> Bool
theElem [] _ = False
theElem xs x = if (x /= head xs)
                then theElem (tail xs) x
                else True

-- 2
-- theLast: return last element of a non-empty list
-- use if-then-else
-- theLast [1..3] -> 3
theLast :: Eq a => [a] -> a
theLast xs = if (tail xs == [])
                 then head xs
                 else theLast (tail xs)

-- 3
-- theLenght: return length of a list
-- use if-then-else
-- thelenght [1..100] -> 100
theLenght :: Eq a => [a] -> Int
theLenght xs = if (tail xs == [])
               then 1
               else 1 + (theLenght (tail xs))
-- 4
-- theNth: return index n of xs
-- use pattern-matching
-- theNth 2 [12..15] -> 14
theNth :: Eq a => Int -> [a] -> a
theNth 0 (x:_) = x
theNth n (x:xs) = theNth (n - 1) xs
-- nth: check index is in bounds and non-negative, then call theNth
-- use guards
nth :: Ord a => Int -> [a] -> a
nth n xs | n < 0 = error "index below 0"
         | n >= (theLenght xs) = error "index bigger than array"
         | otherwise = theNth n xs

-- 5
-- remove: delete all occurences of n in xs
-- remove 4 ([1..5] ++ [1..5]) -> [1,2,3,5,1,2,3,5]
remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x xs | head xs == x = remove x (tail xs)
            | otherwise = (head xs) : (remove x (tail xs))

-- 6
-- subst: replace all m with n in xs
-- subst 4 5 ([1..5] ++ [1..5]) -> [1,2,3,5,5,1,2,3,5,5]
subst :: Eq a => a -> a -> [a] -> [a]
subst _ _ [] = []
subst m n xs | head xs == m = n : subst m n (tail xs)
             | otherwise = (head xs) : (subst m n (tail xs))

-- 7
-- rev: reverse a list
-- use pattern matching
-- rev [1..3] -> [3, 2, 1]
rev :: [a] -> [a]
rev [] = []
rev xs = last xs : (rev (init xs))

-- 8
-- append: concatenate two lists xs,ys returned as a new list
-- use pattern matching, and leave xs,ys unchanged (??)
-- FIXME?
-- append [1..3] [4..6] -> [1..6]
append :: [a] -> [a] -> [a]
append [] [] = []
append [] ys = (head ys) : (append [] (tail ys))
append xs ys = (head xs) : (append (tail xs) ys)

-- 9
-- normal: make a list to a set
-- normal [] -> []
-- normal [1, 1, 2, 2, 3, 3, 3] -> [1, 2, 3]
-- fixme: use general type instead of Int
normal :: [Int] -> [Int]
normal [] = []
normal xs = (head xs) : (normal (remove (head xs) (tail xs)))

-- 10
-- inter: intersect xs ys
{-inter :: [Int] -> [Int]-}
{-inter [] _ = []-}
{-inter _ [] = []-}
{--- fixme-}
{-inter xs ys = [k | ]-}

-- 11
-- theUnion: create union 
