--
-- lab 1.4
--
module EinsVier where

-- 1
-- fibo: calculate fibonacci number
-- fibo 0 -> 0
-- fibo 4 -> 5
-- fibo 5 -> 8
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-- 2
-- fiboTwo: return (predecessor, fibonacci number), works only for values 1 and above
-- fiboTwo 5 -> (5, 8)
-- fiboTwo 6 -> (8, 13)
fiboTwo :: Int -> (Int, Int)
fiboTwo n = fiboTwoHelp n 1 (0, 1)
fiboTwoHelp :: Int -> Int -> (Int, Int) -> (Int, Int)
fiboTwoHelp n i (a, b) | n == i = (a, b)
                       | otherwise = fiboTwoHelp n (i + 1) (step (a, b))
-- fiboTwo HAS to use the next function, part of the task
step :: (Int, Int) -> (Int, Int)
step (x, y) = (y, (x + y))
superFibo :: Int -> Int
superFibo n = snd (fiboTwo n)

-- 3
-- 
