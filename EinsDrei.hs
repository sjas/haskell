--
-- lab 1.3
--
module EinsDrei where

-- 1
-- natural: 1 returns 0, 2 returns 1, ... etc.
naturalRec :: Integer -> Integer
naturalRec 1 = 0
naturalRec x = 1 + naturalRec (x - 1)

-- 2
-- odd: 1->1, 2->3, 3->5, ... etc.
oddRec :: Integer -> Integer
oddRec 1 = 1
oddRec x = 2 + oddRec (x - 1)

-- 3
-- sumRec: sum of the numbers 1..n
-- sumRec 4 -> 10
-- sumRec 5 -> 15
sumRec :: Integer -> Integer
sumRec 0 = 0
sumRec n = n + sumRec (n - 1)

-- 4
-- factRec: n! implemented
-- factRec 5 -> 120
factRec :: Integer -> Integer
factRec 0 = 1
factRec n = n * factRec (n-1)

-- 5
-- sumFact: sum from i=0 to n over i!
-- sumFact 4 -> 153
sumFact :: Integer -> Integer
sumFact 0 = 1
sumFact n = factRec n + sumFact (n - 1)

-- 6
-- arithmeticSumRec: same as arithmeticSum in 1.1(7), in theory
-- IS THE TESTCODE CORRECT??
-- arithmeticSumRec 2 3 4 -> 32
-- for more testing, compare results to the ones of the already implemented function
arithmeticSumRec :: Integer -> Integer -> Integer -> Integer
arithmeticSumRec a n = arithmeticSumRecHelp a (n - 1)
-- helper function, so algorithm is easier to implement
arithmeticSumRecHelp :: Integer -> Integer -> Integer -> Integer
arithmeticSumRecHelp a 0 _ = a
arithmeticSumRecHelp a n d = (a + (n * d)) + arithmeticSumRecHelp a (n - 1) d

-- 7
-- multRec: multiply two Integer's, also negative ones
-- multRec 7 6 -> 42
-- multRec (-7) 6 -> (-42)
-- multRec (-7) (-6) -> 42
multRec :: Integer -> Integer -> Integer
multRec 0 _ = 0
multRec _ 0 = 0
multRec m n | m < 0 && n < 0 = multRecHelp (absoluteValHelp m) (absoluteValHelp n)
            | m < 0 || n < 0 = switchPrefixHelp (multRecHelp (absoluteValHelp m) (absoluteValHelp n))
            | otherwise = multRecHelp m n
-- absolutevalhelp: return absolute value of an Integer
-- absolutevalhelp 1 -> 1
-- absolutevalhelp (-1) -> 1
absoluteValHelp :: Integer -> Integer
absoluteValHelp x | x < 0 = -x
                  | otherwise = x
-- switchPrefixHelp: switch existing prefix
-- switchPrefixHelp 1 -> (-1)
-- switchPrefixHelp (-1) -> 1
switchPrefixHelp :: Integer -> Integer
switchPrefixHelp x = -x
multRecHelp :: Integer -> Integer -> Integer
multRecHelp _ 0 = 0
multRecHelp m n = m + multRecHelp m (n - 1)

-- 8
-- rangeProduct: two Integer's specify a range, multiply them all, '0' if m > n
-- rangeProduct 8 3 -> 0 
-- rangeProduct 3 5 -> 60
-- rangeProduct -1 1 -> 0
rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n | m > n = 0
                 | m == n = m
                 | otherwise = multRec m (rangeProduct (m + 1) n)

-- 9
-- intSqr: find largest possible square fitting in a given Integer
-- intSqr 15 -> 3
-- intSqr 16 -> 4
intSqr :: Integer -> Integer
intSqr n = intSqrHelp n 1
-- intSqrHelp: helper function to find maximal sqr Integer
intSqrHelp :: Integer -> Integer -> Integer
intSqrHelp m n | m == 0 = 0 -- added 0 as possibility, so function can be used in ex.10
               | m == n = 1
               | (n * n) > m = n - 1
               | otherwise = intSqrHelp m (n + 1)

-- 10
-- maxfRec: given an (Integer -> Integer) function f and an Integer n, find the n maximum values of f x
-- intSqr will here be used, as f.
-- maxfRec (
maxfRec :: (Integer -> Integer) -> Integer -> Integer
{-maxfRec f n | n == 0 = f 0-}
            {-| otherwise = max (f n) (maxfRec f (n - 1))-}
maxfRec f 0 = f 0
maxfRec f n = max (f n) (maxfRec f (n - 1))

--11
-- oneZero: given an (Integer -> Integer) function f and an Integer n, return True if one or more values of f 0 .. f n return 0
-- use different of the above functions, to test for true or false
oneZero :: (Integer -> Integer) -> Integer -> Bool
oneZero f n | n == 0 = f 0 == 0
            | otherwise = (f n == 0) || oneZero f (n-1)
