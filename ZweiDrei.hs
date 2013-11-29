--
-- lab 2.3
--
module ZweiDrei where

-- 1
allLessThan :: Ord a => a -> [a] -> Bool
allLessThan n = all (< n)
                
-- 2
anyEmpty :: [[Int]] -> Bool
anyEmpty xs = length xs /= length (filter (/= []) xs)

-- 3
theSame :: String -> Bool
theSame (x:xs) = [] == filter (/= x) xs
                 
-- 4
twice :: (Integer -> Integer) -> Integer -> Integer
twice f n = f $ f n

-- 5
theAll :: (a -> Bool) -> [a] -> Bool
theAll _ [] = True
theAll f (x:xs) = f x && theAll f xs

-- 6
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [ f x | x <- xs ]

-- 7
myRecMap :: (a -> b) -> [a] -> [b]
myRecMap _ [] = []
myRecMap f (x:xs) = f x : myRecMap f xs
                    
-- 8
addDashes :: [String] -> [String]
addDashes = map ((++ "'").("'" ++))

-- 9
shiftPairs :: [(a, b)] -> [(b, a)]
shiftPairs = map (\x -> (snd x, fst x))
             
-- 10
applyEach :: [(Integer -> Integer, Integer)] -> [Integer]
applyEach = map (\ x -> fst x (snd x))
            
-- 11
long :: Num a => [a] -> a
long xs = sum (map (\ x -> 1) xs)

-- 12
iter :: Integer -> (Integer -> Integer) -> Integer -> Integer
iter 1 f x = f x
iter n f x = iter (n-1) f (f x)

-- 13
myFilterRec :: (a -> Bool) -> [a] -> [a]
myFilterRec _ [] = []
myFilterRec f (x:xs) | f x = x : myFilterRec f xs
                     | otherwise = myFilterRec f xs

-- 14
sumSquares :: Integer -> Integer
sumSquares 0 = 0
sumSquares 1 = 1
sumSquares n = foldr (+) 0 (map (\ x -> x * x) [1..n])
               
-- 15
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip xs = ( (foldr (\ x -> (fst x :)) [] xs), (foldr (\ x -> (snd x :)) [] xs) )
-- myUnzip xs = foldr (\ x -> ((fst x :),(snd x :))) ([],[]) xs
-- myUnzip xs = ( map fst xs, map snd xs )
