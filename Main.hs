--
-- This file contains tests for the haskell files in the same folder.
--
module Main (main) where

import Test
import Deprecated

import EinsEins
import EinsZwei
import EinsDrei
import EinsVier()

import ZweiEins
import ZweiZwei
import ZweiDrei
    
import DreiEins
import DreiZwei

-- import PracticePaper

main :: IO ()
main 
    = do 

    print "Starting Tests..."

    -- 1.1
    chk (double 0 == 0)
    chk (double 2 == 4)
    chk (ratio 2.5 1.5 == 4.0)
    chk (hyp 3 4 == 5)
    chk (xIntercept 0.5 6 3 == -6)
    chk (threediff 1 2 3)
    chk (averageThree 3 4 10 == 5.6666665)
    chk (arithmeticSum 2 3 4 == 18)
    chk (not (inRange1 1 2 3))
    chk (inRange1 2 1 3)
    chk (inRange1 2 3 1)
    chk (orExclusive True False)
    chk (not (orExclusive True True))
    chk (not (orExclusive False False))
    chk (not (implies True False))
    chk (implies False True)
    chk (implies True True)
    chk (implies False False)
    chk (hundreds 1234 == 2)
    chk (hundreds 24 == 0)
    chk (hundreds 321 == 3)
    chk (isSmall 'a')
    chk (not (isSmall 'B'))
    chk (upperCase 'a' == 'A')
    chk (upperCase 'B' == 'B')
    chkk (middle "wxAyz") 'A'
    chkk (middle "wxBy") 'B'
    chkk (substring "asdfaXYZa" "XYZ") True
    chkk (substring "asdfaXZa" "XYZ") False

    -- 1.2
    chkk (convertSecToMin 80) (1,20)
    chkk (convertMinToSec (1,20)) 80
    chkk (convertMinToSec (convertSecToMin 3333)) 3333
    chkk (sumUpTimeHelper [(12,59)]) (convertMinToSec (12,59))
    chkk (sumUpTime [(5,18), (3, 27), (3, 25)]) (12, 10)

    -- 1.3
    chkk (naturalRec 20) 19
    chkk (oddRec 4) 7
    chkk (oddRec 6) 11
    chkk (sumRec 4) 10
    chkk (sumRec 5) 15
    chkk (factRec 0) 1
    chkk (factRec 1) 1
    chkk (factRec 2) 2
    chkk (factRec 3) 6
    chkk (factRec 4) 24
    chkk (factRec 5) 120
    chkk (sumFact 0) 1
    chkk (sumFact 1) 2
    chkk (sumFact 2) 4
    chkk (sumFact 3) 10
    chkk (sumFact 4) 34
    chkk (sumFact 5) 154
    chkk (arithmeticSum    2 3 4) 18
    chkk (arithmeticSumRec 2 3 4) 18
    chkk (arithmeticSumRec 22 33 44) (arithmeticSum 22 33 44)
    chkk (absoluteValHelp 1) 1
    chkk (absoluteValHelp (-1)) 1
    chkk (switchPrefixHelp 1) (-1)
    chkk (switchPrefixHelp (-1)) 1
    chkk (multRec 7 6) 42
    chkk (multRec (-7) 6) (-42)
    chkk (multRec (-7) (-6)) 42
    chkk (rangeProduct 8 3) 0
    chkk (rangeProduct 3 5) 60
    chkk (rangeProduct (-1) 1) 0
    chkk (rangeProduct (-5) (-3)) (-60)
    chkk (rangeProduct (-5) (-2)) 120
    chkk (intSqr 1) 1
    chkk (intSqr 2) 1
    chkk (intSqr 3) 1
    chkk (intSqr 4) 2
    chkk (intSqr 5) 2
    chkk (intSqr 15) 3
    chkk (intSqr 16) 4
    chkk (maxfRec intSqr 0 ) 0
    chkk (maxfRec intSqr 9 ) 3
    chkk (maxfRec intSqr 15) 3
    chkk (maxfRec intSqr 16) 4
    chkk (oneZero sumRec 10) True
    chkk (oneZero factRec 10) False

    -- 1.4 TOWERS OF HANOI
    -- chk (hanoi 0 == "")
    -- chkk (hanoi 2) [('a', 'b'), ('a', 'c'), ('b', 'c')]

    -- 2.1
    chkk (isEven 1) False
    chkk (isEven 0) True
    chkk (isEven (-2)) True
    chkk (listEvens 13 29) [28, 26, 24, 22, 20, 18, 16, 14]
    chkk (listEvens 20 31) [30, 28, 26, 24, 22, 20]
    chkk (listEvens 31 20) []
    chkk (listEvens (-29) (-22)) [-22, -24, -26, -28]
    chkk (listTables 4) [(0,0,0),(0,1,0),(0,2,0),(0,3,0),(0,4,0),(1,0,0),(1,1,1),(1,2,2),(1,3,3),(1,4,4),(2,0,0),(2,1,2),(2,2,4),(3,0,0),(3,1,3),(4,0,0),(4,1,4)]
    chkk (addPairwise [1, 2] [3, 4, 5]) [4, 6]
    chkk (addPairwise [] [1, 2, 3]) []
    chkk (addPairwise [-7, -8, -9] [10, 11, 12]) [3, 3, 3]
    chkk (subList [0, 1, 2, 3, 4, 5] (2, 4)) [2, 3, 4]
    chkk (subList [0, 1, 2, 3, 4, 5] (2, 2)) [2]
    chkk (subList ['a', 'b', 'c', 'd', 'e'] (1, 3)) ['b', 'c', 'd']
    chkk (subList' [0, 1, 2, 3, 4, 5] (2, 4)) [2, 3, 4]
    chkk (subList' [0, 1, 2, 3, 4, 5] (2, 2)) [2]
    chkk (subList' ['a', 'b', 'c', 'd', 'e'] (1, 3)) ['b', 'c', 'd']
    chkk (together [1, 2, 3, 4]) [(1, 2), (2, 3), (3, 4)]
    chkk (together ['a', 'b', 'c']) [('a', 'b'), ('b', 'c')]
    chkk (together ["compass", "name", "line"]) [("compass", "name"), ("name", "line")]

    -- 2.2
    chk  (theElem [2..22] 17)
    chk  (not (theElem [2..22] 1))
    chkk (theLast [1..3]) 3
    chkk (theLenght [1..100]) 100
    chkk (theNth 2 [12..15]) 14
    chkk (nth 2 [12..15]) 14
    chkk (remove 4 ([1..5] ++ [1..5])) [1,2,3,5,1,2,3,5]
    chkk (subst 4 5 ([1..5] ++ [1..5])) [1,2,3,5,5,1,2,3,5,5]
    chkk (rev [1..3]) (reverse [1..3])
    chkk (append [1..3] [4..6]) [1..6]
    chkk (normal []) []
    chkk (normal [1, 1, 2, 2, 3, 3, 3]) [1, 2, 3]
    chkk (inter [1..10] [5..20]) [5..10]
    chkk (theUnion [1..10] [5..20]) [1..20]

    -- 2.3
    chk  (allLessThan 55.3 [22..33])
    chk  (anyEmpty [ [1..3], [2..3], [] ])
    chk  (not (anyEmpty [ [1..3], [2..3], [2] ]))
    chk  (not (theSame "asdf"))
    chk  (theSame "aaa")
    chkk (twice (+ 1) 1) 3
    chk  (theAll isEven [2,4..10])
    chkk (myMap (+ 1) [1..3]) (map (+ 1) [1..3])
    chkk (myRecMap (+ 1) [1..3]) (map (+ 1) [1..3])
    chkk (addDashes ["a", "b"]) ["'a'", "'b'"]
    chkk (shiftPairs [("two", "one"), ("bye", "hi")]) [("one", "two"), ("hi", "bye")]
    chkk (shiftPairs [(4, 5), (10, 9), (12, 3)]) [(5, 4), (9, 10), (3, 12)]
    chkk (applyEach [(superFibo, 3), (factRec, 5), (double, 7)]) [2, 120, 14]
    chkk (applyEach [(sumFact, 4), (\x -> x * 3, 16)]) [34, 48]
    chkk (long [1..3]) (length [1..3])
    chkk (iter 3 (+ 1) 0) 3
    chkk (iter 1 superFibo 6) (superFibo 6)
    chkk (iter 2 superFibo 6) (superFibo (superFibo 6))
    chkk (iter 3 superFibo 6) (superFibo (superFibo (superFibo 6)))
    chkk (myFilterRec isEven [1..5]) [2, 4]
    chkk (sumSquares 3) 14
    chkk (myUnzip (zip [1..10] [11..20])) ([1..10], [11..20])
    -- print (myUnzip (zip [1..10] [11..20]))
         
    -- 3.1
    chkk theSeasons [Spring,Summer,Autumn,Winter]
    chkk (seasonsFrom Autumn) [Autumn, Winter] 
    chkk (seasonsFrom Summer) [Summer, Autumn, Winter]
    chkk (listSeasonsFrom [Summer]) [[Summer, Autumn, Winter]]
    chkk (listSeasonsFrom [Winter, Summer]) [[Winter], [Summer, Autumn, Winter]]
    chkk (listSeasonsFrom [Autumn, Winter, Summer]) [[Autumn, Winter], [Winter], [Summer, Autumn, Winter]]
    chkk (month2season Jan) Winter
    chkk (month2season Jul) Summer
    chkk (season2months Winter) [Jan, Feb, Dec]
    chkk (season2months Summer) [Jun, Jul, Aug]
    -- mapM_ print (map season2months theSeasons)     
    chkk (monthANDseason []) []
    chkk (monthANDseason [Mar, Feb]) [(Mar, Spring), (Feb, Winter)]
    chkk (monthANDseason [Jan, Apr, Jul, Oct]) [(Jan, Winter), (Apr, Spring), (Jul, Summer), (Oct, Autumn)]
    chkk (bool2MyBoolean True) T
    chkk (bool2MyBoolean False) F
    chkk (oder F F) F
    chkk (oder F T) T
    chkk (und T T) T
    chkk (und F T) F
    chkk (allUnd []) T
    chkk (allUnd [T, T]) T
    chkk (allUnd [T, F]) F
    chkk (allOder []) F
    chkk (allOder [F, F]) F
    chkk (allOder [T, F]) T
    chkk (convert []) 0
    chkk (convert [I,O,I]) 5
    chkk (convert [O,I,I,I]) 7

    -- 3.2
    chkk (rounded (Exact 10)) 10
    chkk (rounded (Approx 10.9)) 11
    chkk (rounded (Approx (-23))) (-23)
    chkk (firstName (Person (Nomen "Ada" "Lovelace") (Years 36))) "Ada"
    chkk (howOld (Person (Nomen "Haskell" "Curry") (Years 81))) (Years 81)
    chkk (addAges (Person (Nomen "A" "L") (Years 10)) (Person (Nomen "X" "Y") (Years 12))) (Years 22)
    chkk (isRound (Circle pi)) T
    chkk (isRound (Rectangle 10.5 12.3)) F


    

    -- -- Practice Paper
    -- -- 1a
    -- chkk (isaprefix [] [1..10]) True
    -- chkk (isaprefix [5, 6, 7] [5, 6, 7, 8, 9]) True
    -- chkk (isaprefix [5, 6, 7] [4, 5, 6, 7, 8, 9]) False
    -- chkk (isaprefix ["Hello", "Haskell"] ["Hello", "Haskell", "F2"]) True
    -- -- 1b
    -- chkk (isasublist [1, 2, 3] [0, 1, 2, 3, 4]) True
    -- chkk (isasublist [1, 2, 3] [1, 2, 10, 2, 3, 11]) False
    -- chkk (isasublist "Chip" "Fish&Chips") True
    -- -- 1c
    -- chkk (composeFun [fun1, fun2] 10) 22
    -- chkk (composeFun [fun2, fun1] 10) 21
    -- chkk (composeFun [] 10) 10
