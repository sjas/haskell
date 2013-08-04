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

