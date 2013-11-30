
-- lab 3.2
--
module DreiZwei where

import DreiEins (MyBoolean(T,F))

-- 1
data Number = Exact Integer | Approx Float
rounded :: Number -> Integer
rounded (Exact n) = n
rounded (Approx n) = round n

-- 2
data Age = Years Integer deriving (Eq, Show)
data Name = Nomen String String deriving (Show)
data People = Person Name Age deriving (Show)
firstName :: People -> String
firstName (Person (Nomen first family) _) = first
howOld :: People -> Age
howOld (Person _ (Years alter)) = (Years alter)
getAge :: Age -> Integer
getAge (Years years) = years
addAges :: People -> People -> Age
addAges x y = (Years ((s x) + (s y)))
              where 
                s = (\a -> getAge $ howOld a)

-- 3
data Shape = Circle Float | Rectangle Float Float deriving (Show)
               
-- 4
isRound :: Shape -> MyBoolean
isRound (Circle _) = T
isRound (Rectangle _ _) = F

-- 5
getArea :: Shape -> Float
getArea (Rectangle a b) = a * b

-- 6
data Point = Pair Float Float deriving (Show)
dist :: Point -> Point -> Float
dist (Pair x1 y1) (Pair x2 y2) = sqrt ( (y2-y1)**2 + (x2-x1)**2 )

-- 7
data Slope = Value Float | Infinite deriving (Eq, Show)
getSlope :: Point -> Point -> Slope
getSlope (Pair x1 y1) (Pair x2 y2) | x1 == x2 = Infinite
                                   | otherwise = ( Value ( (y2 - y1) / (x2-x1) ) )

-- 8
data Y_intercept = Intercept Float | Undefined deriving (Eq, Show)
getYintercept :: Point -> Slope -> Y_intercept
getYintercept _ Infinite = Undefined
getYintercept (Pair x y) (Value m) = (Intercept (y - m * x))

-- 9
data Figure = Place Shape Point deriving (Show)
move :: Float -> Float -> Figure -> Figure 
move a b (Place s (Pair x y)) = (Place s (Pair (a+x) (b+y)))

-- 10
shape2figure :: Shape -> Point -> Figure
shape2figure s p = (Place s p)

-- -- 11
-- overlap :: Figure -> Figure -> Bool
-- overlap 

-- -- 12
-- data Direction = West | East | North | South deriving (Eq,Show)
-- data Speed = Km Float deriving (Show)
-- data UFO = Ufo Point Slope Direction Speed deriving (Show)
-- -- TODO implement
-- data Time = Secs Float deriving (Show)
-- predict :: UFO -> Time -> Point
-- predict ufo t = willBe (adjust ufo) t
-- -- TODO implement
