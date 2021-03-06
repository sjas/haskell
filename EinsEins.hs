--
-- lab 1.1
--
module EinsEins where
import Data.Char (toUpper)

-- 1
-- double: double value of an integer
-- double 2 -> 4
double :: Integer -> Integer
double a = 2 * a

-- 2
-- ratio: take two real numbers x,y and return ratio of x+y to x-y
-- ratio 2.5 1.5 -> 4.0
ratio :: Float -> Float -> Float
ratio x y = (x + y) / (x - y)

-- 3
-- hyp: calculate hypothenuse from two cathetes
-- hyp 3 4 -> 5
hyp :: Float -> Float -> Float
hyp k1 k2 = sqrt (k1**2 + k2**2)

-- 4
-- xIntercept: given a linear equation, calculate the point where the graph crosses the abscissa
-- xIntercept 0.5 6 3 -> -6
xIntercept :: Float -> Float -> Float -> Float
xIntercept m c y = (y - c) / m

-- 5
-- threediff: return True if a,b,c are all different from each other
-- threediff 1 2 3 -> True
threediff :: Integer -> Integer -> Integer -> Bool
threediff m n p | m == n = False
                | m == p = False
                | n == p = False
                | otherwise = True

-- 6
-- averageThree: return average of three Integer as Float
-- averageThree 3 4 10 -> 5.6666665
averageThree :: Integer -> Integer -> Integer -> Float
-- regular
{-averageThree a b c = (fromIntegral (a + b + c)) / 3-}
-- polish
averageThree a b c = ( fromIntegral ((+) ((+) a b) c) ) / 3

-- 7
-- arithmeticSum: sum up, use formula equation, dont implement sum stuff yourself
-- arithmeticSum 2 3 4 -> 18
-- THE IMPLEMENTATION IS CORRECT
-- BUT MAYBE THE CALCULATION BEFORE NOT
arithmeticSum :: Integer -> Integer -> Integer -> Integer
arithmeticSum a n d = (n * ((2 * a) + ((n - 1) * d))) `div` 2

-- 8
-- inRange1: check if x lies between a,b or b,a
-- inRange1 1 2 3 -> False
-- inRange1 2 1 3 -> True
-- inrange1 2 3 1 -> True
inRange1 :: Integer -> Integer -> Integer -> Bool
inRange1 x a b | x > a && x < b = True
               | x < a && x > b = True
               | otherwise = False

-- 9
-- orExclusive: implement XOR
-- orExclusive True False -> True
-- orExclusive True True -> False
-- orExclusive False False -> False
orExclusive :: Bool -> Bool -> Bool
orExclusive a b | a == True && a /= b = True
                | a == False && a /= b = True
                | otherwise = False

-- 10
-- implies: implement logical 'imples'
-- implies True False -> False
--    otherwise True
implies :: Bool -> Bool -> Bool
-- BAD STYLE
-- implies a b | a == True && b == False = False
--             | otherwise = True
-- GOOD STYLE
implies True False = False
implies _ _        = True

-- 11
-- hundreds: take an Integer and return to '100' digit, else 0
-- hundreds 1234 -> 2
-- hundreds 24 -> 0
-- hundreds 321 -> 3
hundreds :: Integer -> Integer
hundreds a | a < 100 = 0
           | otherwise = div (rem a 1000) 100

-- 12
-- upperCase: convert a lowercase Char to uppercase, else leave unchanged and return
-- upperCase a -> A
-- upperCase B -> B
-- uppercase 2 -> error
-- HELPER FUNCTION:
-- isSmall: returns True if argument is lowercase Char
isSmall :: Char -> Bool
isSmall x = 'a' <= x && x <= 'z'
upperCase :: Char -> Char
upperCase a | isSmall a = toUpper a
            | otherwise = a

-- 13
-- middle: return middle char of non-empty string
-- middle "wxAyz" -> 'A'
-- middle "wxBy" -> 'B'
middle :: String -> Char
middle xs = xs !! div (length xs) 2

-- 14
-- substring: return true if string b is substring of a, a has no duplicated elements
-- substring "asdfaXYZa" "XYZ" -> True
-- substring "asdfaXZa" "XYZ" -> False
substring :: String -> String -> Bool
substring xs ys = and [ y `elem` xs | y <- ys]
