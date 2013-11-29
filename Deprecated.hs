--
-- DEPRECATED CODE HERE, MAYBE GOT USED IN THE ASSIGNMENTS
--
module Deprecated where

-- 1
-- fibo: calculate fibonacci number
-- fibo 0 -> 0
-- fibo 4 -> 5
-- fibo 5 -> 8
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- 2
-- fiboTwo: return (predecessor, fibonacci number), works only for values 1 and above
-- fiboTwo 5 -> (5, 8)
-- fiboTwo 6 -> (8, 13)
fiboTwo :: Integer -> (Integer, Integer)
fiboTwo n = fiboTwoHelp n 1 (0, 1)
fiboTwoHelp :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
fiboTwoHelp n i (a, b) | n == i = (a, b)
                       | otherwise = fiboTwoHelp n (i + 1) (step (a, b))
-- fiboTwo HAS to use the next function, part of the task
step :: (Integer, Integer) -> (Integer, Integer)
step (x, y) = (y, x + y)
superFibo :: Integer -> Integer
superFibo n = snd (fiboTwo n)

-- 3
-- sumFunction: take a function f and a value n as arguments, and return the sum of f 0 .. f n
-- sumFunction intSqr 1 -> 1
-- sumFunction intSqr 4 -> 5
-- sumFunction intSqr 15 -> 10
sumFunction :: (Integer -> Integer) -> Integer -> Integer
sumFunction f 0 = f 0
sumFunction f n = f n + sumFunction f (n - 1)

--reverseList: for given xs return reversed list
--reverseList [1,2,3] -> [3,2,1]
reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- asdf
-- DEPRECATED TESTCODE BELOW HERE
--
--    chkk (reverseList [1, 2, 3]) [3, 2, 1]
--
--    -- 1.4
--    chkk (fibo 0) 0
--    chkk (fibo 1) 1
--    chkk (fibo 2) 1
--    chkk (fibo 3) 2
--    chkk (fibo 4) 3
--    chkk (fibo 5) 5
--    chkk (fibo 6) 8
--    chkk (fiboTwo 1) (0, 1)
--    chkk (fiboTwo 2) (1, 1)
--    chkk (fiboTwo 3) (1, 2)
--    chkk (fiboTwo 4) (2, 3)
--    chkk (fiboTwo 5) (3, 5)
--    chkk (fiboTwo 6) (5, 8)
--    chkk (superFibo 1) 1
--    chkk (superFibo 2) 1
--    chkk (superFibo 3) 2
--    chkk (superFibo 4) 3
--    chkk (superFibo 5) 5
--    chkk (superFibo 6) 8
--    chkk (sumFunction intSqr 1) 1
--    chkk (sumFunction intSqr 4) 5
--    chkk (sumFunction intSqr 5) 7
