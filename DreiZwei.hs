
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
data Shape = Circle Float | Rectangle Float Float
               
-- 4
isRound :: Shape -> MyBoolean
isRound (Circle _) = T
isRound (Rectangle _ _) = F
