--
-- lab 3.1
--
module DreiEins where

data Season = Spring | Summer | Autumn | Winter deriving (Eq, Show, Enum)

-- 1
theSeasons :: [Season]
theSeasons = [Spring, Summer, Autumn, Winter]
    
-- 2
seasonsFrom :: Season -> [Season]
seasonsFrom = seasonsFromHelp theSeasons
seasonsFromHelp :: [Season] -> Season -> [Season]
seasonsFromHelp [] _ = []
seasonsFromHelp (x:xs) s | s == x = x:xs
                         | otherwise = seasonsFromHelp xs s

-- 3
listSeasonsFrom :: [Season] -> [[Season]]
listSeasonsFrom = map seasonsFrom

-- 4
data Month = Jan | Feb |Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Ord, Show, Enum)
theMonths :: [Month]
theMonths = [Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec]

month2season :: Month -> Season
month2season x | Mar <= x && x <= May = Spring
               | Jun <= x && x <= Aug = Summer
               | x <= Mar || x == Dec = Winter
               | otherwise = Autumn

-- 5
season2months :: Season -> [Month]
season2months s = [ m | m <- theMonths, month2season m == s ]

-- 6
monthANDseason :: [Month] -> [(Month,Season)]
monthANDseason = map (\ x -> (x, month2season x))

-- 7
-- data MyBoolean = F | T deriving (Show)
data MyBoolean = F | T deriving (Eq, Show)
bool2MyBoolean :: Bool -> MyBoolean
bool2MyBoolean x | x = T
                 | otherwise = F

-- 8
und :: MyBoolean -> MyBoolean -> MyBoolean
und T T = T
und _ _ = F
oder :: MyBoolean -> MyBoolean -> MyBoolean
oder F F = F
oder _ _ = T

-- 9
allUnd :: [MyBoolean] -> MyBoolean
allUnd = foldr und T
allOder :: [MyBoolean] -> MyBoolean
allOder = foldr oder F

-- 10
data Bit = I | O deriving (Eq, Show)
convert :: [Bit] -> Integer
convert xs = calculateDecFromBin (reverse [ if x == I then 1 else 0 | x <- xs ]) 0
calculateDecFromBin :: [Integer] -> Integer -> Integer
calculateDecFromBin [] _ = 0
calculateDecFromBin (x:xs) n | x == 1 = 2^n + calculateDecFromBin xs n+1
                             | otherwise = calculateDecFromBin xs n+1
