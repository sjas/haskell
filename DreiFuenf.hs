--
-- lab 3.5
--
module DreiFuenf where

-- 1
data Pairs a = Duple a a
swapPair :: Pairs a -> Pairs a
swapPair (Duple a b) = Duple b a
eqPair :: Eq a => Show a => Pairs a -> Bool
eqPair (Duple a b) | a == b = True
                   | otherwise = False

-- 2
data List a = NilList | Cons a (List a) deriving (Eq,Ord,Show,Read)
emptyList :: List a -> Bool
emptyList NilList = True
emptyList _ = False
lengthList :: List a -> Integer
lengthList NilList = 0
lengthList (Cons _ a) = 1 + lengthList a
