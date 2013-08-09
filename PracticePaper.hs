--
-- practice paper
--
module PracticePaper where

-- 1a
-- isaprefix: check if ys starts with xs
-- isaprefix [] [1..10] -> True
-- isaprefix [5, 6, 7] [5, 6, 7, 8, 9] -> True
-- isaprefix ["Hello", "Haskell"] ["Hello", "Haskell", "F2"] -> True
isaprefix :: Eq a => [a] -> [a] -> Bool
isaprefix [] _ = True
isaprefix (x:xs) (y:ys) = (x == y) && isaprefix xs ys
-- 1b
-- isasublist: check if xs is sublist of ys
-- isasublist [1, 2, 3] [0, 1, 2, 3, 4] -> True
-- isasublist [1, 2, 3] [1, 2, 10, 2, 3, 11] -> False
-- isasublist "Chip" "Fish&Chips" -> True
isasublist :: Eq a => [a] -> [a] -> Bool
isasublist [] _ = True
isasublist (x:xs) (y:ys) | x == y = isaprefix xs ys
                         | otherwise = isasublist (x:xs) ys

-- 1c
-- composefun: xs is a list of (Int -> Int) applied to x
-- helper functions:
fun1 :: Int -> Int
fun1 x = x + 1
fun2 :: Int -> Int
fun2 x = x * 2
-- composeFun [fun1, fun2] 10 22
-- composeFun [fun2, fun1] 10 21
-- composeFun [] 10 10
composeFun :: [(Int -> Int)] -> Int -> Int
composeFun [] x = x
composeFun (f:fs) x = composeFun fs (f x)
