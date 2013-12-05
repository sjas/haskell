--
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
firstName (Person (Nomen first _) _) = first
howOld :: People -> Age
howOld (Person _ (Years alter)) = Years alter
getAge :: Age -> Integer
getAge (Years years) = years
addAges :: People -> People -> Age
addAges x y = Years (s x + s y)
              where 
                s = getAge . howOld

-- 3
data Shape = Circle Float | Rectangle Float Float deriving (Eq,Show)
               
-- 4
isRound :: Shape -> MyBoolean
isRound (Circle _) = T
isRound (Rectangle _ _) = F

-- 5
getArea :: Shape -> Float
getArea (Rectangle a b) = a * b

-- 6
data Point = Pair Float Float deriving (Eq,Show)
dist :: Point -> Point -> Float
dist (Pair x1 y1) (Pair x2 y2) = sqrt ( (y2-y1)**2 + (x2-x1)**2 )

-- 7
data Slope = Value Float | Infinite deriving (Eq, Show)
getSlope :: Point -> Point -> Slope
getSlope (Pair x1 y1) (Pair x2 y2) | x1 == x2 = Infinite
                                   | otherwise = Value ( (y2-y1) / (x2-x1) )

-- 8
data YIntercept = Intercept Float | Undefined deriving (Eq, Show)
getYintercept :: Point -> Slope -> YIntercept
getYintercept _ Infinite = Undefined
getYintercept (Pair x y) (Value m) = Intercept (y - m * x)

-- 9
data Figure = Place Shape Point deriving (Eq,Show)
move :: Float -> Float -> Figure -> Figure 
move a b (Place s (Pair x y)) = Place s (Pair (a+x) (b+y))

-- 10
shape2figure :: Shape -> Point -> Figure
shape2figure = Place

-- 11
overlap :: Figure -> Figure -> Bool
-- TODO
-- fix this. this is the worst exercise in the first three labs. by far.
-- overlap (Place rect1 p1) (Place rect2 p2) = distCalc p1 p2 < rectCalc p2 p1 rect1 + rectCalc p1 p2 rect2
-- overlap (Place (Circle r) p1) (Place rect p2) = distCalc p1 p2 < r + rectCalc p1 p2 rect
overlap (Place (Circle r1) p1) (Place (Circle r2) p2) = distCalc p1 p2 < r1 + r2

distCalc :: Point -> Point -> Float
-- ^ calculate distance between two given points
distCalc (Pair x1 y1) (Pair x2 y2) = sqrt (getDist x1 x2 ** 2 + getDist y1 y2 **2)
                                     where
                                       getDist a b = max a b - min a b
                                                     
-- this approach wont work (imagine two line-like rectangle forming a triangle with the center connection)
-- rectCalc :: Point -> Point -> Shape -> Float
-- ^ other center -> own center -> own shape -> distance own center to outer bound of own shape
-- rectCalc (Pair x1 y1) (Pair x2 y2) (Rectangle l b) = 0
                                                     

-- 12 FIRST PART 
-- data Point = Pair Float Float deriving (Eq,Show)
-- data Slope = Value Float | Infinite deriving (Eq, Show)
data Direction = West | East | North | South deriving (Eq,Show)
data Speed = Km Float deriving (Eq,Show)
data UFO = Ufo Point Slope Direction Speed deriving (Eq,Show)
adjust :: UFO -> UFO
adjust u @ (Ufo p Infinite direction s) = case direction of
                                        East -> Ufo p Infinite North s
                                        West -> Ufo p Infinite South s
                                        _ -> u
adjust u@(Ufo p v@(Value _) direction s) = case direction of
                                        North -> Ufo p v East s
                                        South -> Ufo p v West s
                                        _ -> u
-- 12 SECOND PART 
data Time = Secs Float deriving (Show)
predict :: UFO -> Time -> Point
predict ufo = willBe (adjust ufo)
willBe :: UFO -> Time -> Point
willBe (Ufo (Pair x y) (Value v) direction (Km k)) (Secs s) = case direction of
                                                                East -> Pair (x + f1) (y + f2)
                                                                West -> Pair (x - f1) (y - f2)
                                                                where
                                                                  f1 = cos (atan v) * k * s
                                                                  f2 = sin (atan v) * k * s
willBe (Ufo (Pair x y) _ direction (Km k)) (Secs s) = case direction of
                                                               North -> Pair x (y + k * s)
                                                               South -> Pair x (y - k * s)
