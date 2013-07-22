--
-- lab 1.2
--
module EinsZwei where

-- convertMinToSec: converts seconds to minutes
-- convertMinToSec (1, 20) -> 80
convertMinToSec :: (Int, Int) -> Int
convertMinToSec (0, 0) = 0
convertMinToSec (a, b) = (a * 60) + b
-- convertSecToMin: converts minutes to seconds
-- convertSectoMin 80 -> (1, 20)
convertSecToMin :: Int -> (Int, Int)
convertSecToMin x = (div x 60, mod x 60)
-- sumUpTime: calculate sum of time
-- sumUpTime [(5,18), (3, 27), (3, 25)]) -> (12, 10)
sumUpTime :: [(Int, Int)] -> (Int, Int)
sumUpTime xs | xs == []         = (0, 0)
             | length xs == 1   = head xs
             | otherwise       = convertSecToMin (sumUpTimeHelper xs)
-- sumUpTimeHelper: almost the same as 'convertMinToSec', except it does take a list of tuples
sumUpTimeHelper :: [(Int, Int)] -> Int
sumUpTimeHelper xs | length xs == 1 = convertMinToSec (head xs)
                   | otherwise = convertMinToSec (head xs) + sumUpTimeHelper (tail xs)

