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
-- factRec: n! implemented
-- factRec 5 -> 120
factRec :: Int -> Int
factRec 0 = 1
factRec n = n * (factRec (n-1))

-- 5
-- sumFact: sum from i=0 to n over i!
-- sumFact 4 -> 153
sumFact :: Int -> Int
sumFact 0 = 1
sumFact n = factRec n + sumFact (n - 1)

-- 6
-- arithmeticSumRec: same as arithmeticSum in 1.1(7), in theory
-- IS THE TESTCODE CORRECT??
-- arithmeticSumRec 2 3 4 -> 32
-- for more testing, compare results to the ones of the already implemented function
arithmeticSumRec :: Int -> Int -> Int -> Int
arithmeticSumRec a n d = arithmeticSumRecHelp a (n - 1) d
-- helper function, so algorithm is easier to implement
arithmeticSumRecHelp :: Int -> Int -> Int -> Int
arithmeticSumRecHelp a 0 d = a
arithmeticSumRecHelp a n d = (a + (n * d)) + (arithmeticSumRecHelp a (n - 1) d)

-- 7
-- multRec: multiply two Int's, also negative ones
-- multRec 7 6 -> 42
-- multRec (-7) 6 -> (-42)
-- multRec (-7) (-6) -> 42
multRec :: Int -> Int -> Int
multRec 0 _ = 0
multRec _ 0 = 0
multRec m n | m < 0 && n < 0 = multRecHelp (absoluteValHelp m) (absoluteValHelp n)
            | m < 0 || n < 0 = switchPrefixHelp (multRecHelp (absoluteValHelp m) (absoluteValHelp n))
            | otherwise = multRecHelp m n
-- absolutevalhelp: return absolute value of an Int
-- absolutevalhelp 1 -> 1
-- absolutevalhelp (-1) -> 1
absoluteValHelp :: Int -> Int
absoluteValHelp x | x < 0 = (-x)
                  | otherwise = x
-- switchPrefixHelp: switch existing prefix
-- switchPrefixHelp 1 -> (-1)
-- switchPrefixHelp (-1) -> 1
switchPrefixHelp :: Int -> Int
switchPrefixHelp x = (-x)
multRecHelp :: Int -> Int -> Int
multRecHelp m 0 = 0
multRecHelp m n = m + (multRecHelp m (n - 1))

-- 8
-- rangeProduct: two Int's specify a range, multiply them all, '0' if m > n
-- rangeProduct 8 3 -> 0 
-- rangeProduct 3 5 -> 60
-- rangeProduct -1 1 -> 0
rangeProduct :: Int -> Int -> Int
rangeProduct m n | m > n = 0
                 | m == n = m
                 | otherwise = multRec m (rangeProduct (m + 1) n)

-- 9
-- intSqr: find largest possible square fitting in a given Int
-- intSqr 15 -> 3
-- intSqr 16 -> 4
intSqr :: Int -> Int
intSqr n = intSqrHelp n 1
-- intSqrHelp: helper function to find maximal sqr Int
intSqrHelp :: Int -> Int -> Int
intSqrHelp m n | m == 0 = 0 -- added 0 as possibility, so function can be used in ex.10
               | m == n = 1
               | ((n * n) > m) = (n - 1)
               | otherwise = intSqrHelp m (n + 1)

-- 10
-- maxfRec: given an (Int -> Int) function f and an Int n, find the n maximum values of f x
-- intSqr will here be used, as f.
-- maxfRec (
maxfRec :: (Int -> Int) -> Int -> Int
maxfRec f n | n == 0 = f 0
            | otherwise = max (f n) (maxfRec f (n - 1))

--11
-- oneZero: given an (Int -> Int) function f and an Int n, return True if one or more values of f 0 .. f n return 0
-- use different of the above functions, to test for true or false
oneZero :: (Int -> Int) -> Int -> Bool
oneZero f n | n == 0 = if (f 0 == 0) then True else False
            | otherwise = (f n == 0) || oneZero f (n-1)
