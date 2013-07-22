--
-- lab 1.3
--
module EinsDrei where

-- 1
-- natural: 1 returns 0, 2 returns 1, ... etc.
naturalRec :: Int -> Int
naturalRec 1 = 0
naturalRec x = 1 + naturalRec (x - 1)

-- 2
-- odd: 1->1, 2->3, 3->5, ... etc.
oddRec :: Int -> Int
oddRec 1 = 1
oddRec x = 2 + (oddRec (x - 1))

-- 3
-- sumRec: sum of the numbers 1..n
-- sumRec 4 -> 10
-- sumRec 5 -> 15
sumRec :: Int -> Int
sumRec 0 = 0
sumRec n = n + sumRec (n - 1)

-- 4
-- facRec: n! implemented
-- facRec 5 -> 120
factRec :: Int -> Int
factRec 0 = 1
factRec n = n * (factRec (n-1))

-- 5
-- sumFact: sum from i=0 to n over fact i
-- sumFact 4 -> 153
sumFact :: Int -> Int
sumFact 0 = 1
sumFact n = factRec n + sumFact (n - 1)
