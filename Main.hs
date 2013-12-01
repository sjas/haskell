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
import DreiDrei
import DreiVier
import DreiFuenf
import DreiSechs
-- import DreiSieben


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
    -- TODO
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
    chkk (getArea (Rectangle 16.9 68)) 1149.2
    chkk (dist (Pair 4 6) (Pair 9 25)) 19.646883
    chkk (getSlope (Pair 1 3) (Pair 10 20)) (Value 1.8888888)
    chkk (getSlope (Pair 2 3) (Pair 1 (-8))) (Value 11.0)
    chkk (getSlope (Pair 100 0) (Pair 100 1)) Infinite
    chkk (getYintercept (Pair 5 5) (Value 1)) (Intercept 0.0)
    chkk (getYintercept (Pair (-5) 25) (Value 0.5)) (Intercept 27.5)
    chkk (getYintercept (Pair 1 1) Infinite) Undefined
    -- testcode not working due to missing 'instance Show' declaration, but implementation tested in REPL and proved to be correct
    -- chkk (move 10 (-10) (Place (Rectangle 3 4) (Pair 0 0))) (Place (Rectangle 3.0 4.0) (Pair 10.0 (-10.0)))
    -- chkk (move 10 (-10) (Place (Rectangle 3 4) (Pair 0 0))) (Place (Rectangle 3.0 4.0) (Pair 10.0 (-10.0)))
    -- chkk (shape2figure (Circle pi) (Pair 1 1)) (Place (Circle 3.141593) (Pair 1.0 1.0))
    -- chkk (shape2figure (Rectangle 1 3) (Pair 10 15)) (Place (Rectangle 1.0 3.0) (Pair 10.0 15.0))
    -- chkk (move 5 5 (shape2figure (Rectangle 1 3) (Pair 10 15))) (Place (Rectangle 1.0 3.0) (Pair 15.0 20.0))

    -- chkk (overlap (Place (Circle 1.414213) (Pair 0 0)) (Place (Circle 1.414213) (Pair 2 2))) False
    -- chkk (overlap (Place (Rectangle 2 8) (Pair 5 0)) (Place (Circle 1) (Pair 0 0))) True
    -- chkk (overlap (Place (Circle 1.414214) (Pair 0 0)) (Place (Circle 1.414214) (Pair 2 2))) True

    -- chkk (adjust (Ufo (Pair 0 0) (Value 1) East (Km 10))) (Ufo (Pair 0.0 0.0) (Value 1.0) East (Km 10.0))
    -- chkk (adjust (Ufo (Pair (-5) 9) Infinite West (Km 1))) (Ufo (Pair (-5.0) 9.0) Infinite South (Km 1.0))
    -- chkk (adjust (Ufo (Pair 1 100) (Value 10) North (Km 200))) (Ufo (Pair 1.0 100.0) (Value 10.0) East (Km 200.0))
    
    -- chkk (predict (Ufo (Pair 0 0) (Value 1) East (Km 1.414214)) (Secs 1)) (Pair 1.0 1.0)
    -- chkk (predict (Ufo (Pair 0 0) (Value 1) North (Km 1.414214)) (Secs 1)) (Pair 1.0 1.0)
    -- chkk (predict (Ufo (Pair 0 0) (Value (-1)) East (Km 1.414214)) (Secs 1)) (Pair 1.0 (-1.0))
    -- chkk (predict (Ufo (Pair 0 0) (Value 1) West (Km 1.414214)) (Secs 1)) (Pair (-1.0) (-1.0))
    -- chkk (predict (Ufo (Pair 0 0) (Value (-1)) West (Km 1.414214)) (Secs 1)) (Pair (-1.0) 1.0)
    -- chkk (predict (Ufo (Pair 1 (-10)) (Value 10000) East (Km 10)) (Secs 1)) (Pair 1.001 0.0)
    -- chkk (predict (Ufo (Pair 5 4) (Value 1) East (Km 1.414214)) (Secs 10)) (Pair 15.0 14.0)
    -- chkk (predict (Ufo (Pair 1 4) Infinite North (Km 10)) (Secs 5)) (Pair 1.0 54.0)
    -- chkk (predict (Ufo (Pair 1 4) Infinite West (Km 10)) (Secs 5)) (Pair 1.0 (-46.0))

    -- 3.3
    chkk (size1 (Lit1 10)) 0
    chkk (size1 (Sub1 (Add1 (Lit1 10) (Lit1 5)) (Lit1 1))) 2
    chkk (size2 (Lit2 10)) 0
    chkk (size2 (Op Sub2 (Op Add2 (Lit2 10) (Lit2 5)) (Lit2 1))) 2
         
    -- 3.4
    chkk (bEval (EQUAL (ILit 1) (SUB (ILit 2) (ILit 1)))) True
    chkk (iEval (IF (EQUAL (ILit 10) (ILit 10)) (ADD (ILit 1) (ILit 2)) (ILit 0))) 3
    chkk (bEval (NOT (EQUAL (ILit 10) (SUB (ILit 11) (ILit 1))))) False
    chkk (iEval (IF (NOT (EQUAL (ILit 3) (SUB (ILit 4) (ILit 1)))) (ILit 1) (ILit 10))) 10

    -- 3.5
    -- chkk (swapPair (Duple 'a' 'b')) (Duple 'b' 'a')
    -- chkk (swapPair (Duple (iEval (ILit 1)) (iEval (ILit 2)))) (Duple 2 1)
    chkk (eqPair (Duple "Haskell" "Type")) False
    chkk (eqPair (Duple False False)) True
    chkk (emptyList NilList) True
    chkk (emptyList (Cons 2 (Cons 1 NilList))) False
    chkk (lengthList (Cons 5 (Cons 4 (Cons 3 (Cons 2 (Cons 1 NilList)))))) 5

    -- 3.6
    chkk (howMany Empty) 0
    chkk (howMany (Node 'a' (Node 'b' (Leaf 'c') Empty) (Leaf 'd'))) 4
    chkk (depth (Leaf 100)) 1
    chkk (depth (Node 'a' (Node 'b' (Leaf 'c') Empty) (Leaf 'd'))) 3
    chkk (reflect (Node 'a' (Node 'b' (Leaf 'c') Empty) (Leaf 'd'))) (Node 'a' (Leaf 'd') (Node 'b' Empty (Leaf 'c')))
    chkk (post (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))) [3,4,2,5,1]
    chkk (post (reflect (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)))) [5,4,3,2,1]
    chkk (pre (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))) [1,2,3,4,5]
    chkk (pre (reflect (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5)))) [1,5,2,4,3]
    chkk (normalise (Node 'a' Empty Empty)) (Leaf 'a')
    chkk (normalise (Node 'a' (Node 'b' Empty (Leaf 'c')) (Node 'd' Empty Empty))) (Node 'a' (Node 'b' Empty (Leaf 'c')) (Leaf 'd'))
    chkk (normalise (Node 1 (Node 2 (Node 3 Empty Empty) (Leaf 4)) (Leaf 5))) (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
    chkk (normalise (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))) (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))
    chkk (mapTree natural (Node 1 (Node (-2) (Leaf 3) (Leaf 4)) (Leaf (-5)))) (Node True (Node False (Leaf True) (Leaf True)) (Leaf False))
    chkk (mapTree plusOne (Node 1 (Node (-2) (Leaf 3) (Leaf 4)) (Leaf (-5)))) (Node 2 (Node (-1) (Leaf 4) (Leaf 5)) (Leaf (-4)))
    -- chkk (lists2tree [1,2,3,4,5] [3,2,4,1,5]) (Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Leaf 5))

         
    -- 3.7
    -- TODO
          

    -- -- practice paper
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
