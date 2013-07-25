--
-- This file contains tests for the haskell files in the same folder.
--
module Main (main) where

import Test

import EinsEins
import EinsZwei
import EinsDrei
import EinsVier
import EinsFuenf

import ZweiEins

main :: IO ()
main 
    = do 

    print "Starting Tests..."

    -- 1.1
    chk(double 0 == 0)
    chk(double 2 == 4)
    chk(ratio 2.5 1.5 == 4.0)
    chk(hyp 3 4 == 5)
    chk(xIntercept 0.5 6 3 == -6)
    chk(threediff 1 2 3 == True)
    chk(averageThree 3 4 10 == 5.6666665)
    chk(arithmeticSum 2 3 4 == 18)
    chk(inRange1 1 2 3 == False)
    chk(inRange1 2 1 3 == True)
    chk(inRange1 2 3 1 == True)
    chk(orExclusive True False == True)
    chk(orExclusive True True == False)
    chk(orExclusive False False == False)
    chk(implies True False == False)
    chk(implies False True == True)
    chk(implies True True == True)
    chk(implies False False == True)
    chk(hundreds 1234 == 2)
    chk(hundreds 24 == 0)
    chk(hundreds 321 == 3)
    chk(isSmall 'a' == True)
    chk(isSmall 'B' == False)
    chk(upperCase 'a' == 'A')
    chk(upperCase 'B' == 'B')
    chk(arrayLength "asdf" == 4)
    chk(arrayLength "" == 0)
    chk(arrayLength "1234567890" == 10)
    -- after I succeeded with a better test method implementation, ofc the new one will be used
    chkk (arrayLength "asdf") 4
    chkk (arrayLength "") 0
    chkk (arrayLength "1234567890") 10
    -- test of internal function 'length'
    chkk (length "1234567890") 10
    chkk (middleIdx "asdfg") 2
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
    -- IS THE TESTCODE CORRECT?
    {-print (arithmeticSum    2 3 4)-}
    {-print (arithmeticSumRec 2 3 4)-}
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

    -- 1.4
    chkk (fibo 0) 0
    chkk (fibo 1) 1
    chkk (fibo 2) 1
    chkk (fibo 3) 2
    chkk (fibo 4) 3
    chkk (fibo 5) 5
    chkk (fibo 6) 8
    chkk (fiboTwo 1) (0, 1)
    chkk (fiboTwo 2) (1, 1)
    chkk (fiboTwo 3) (1, 2)
    chkk (fiboTwo 4) (2, 3)
    chkk (fiboTwo 5) (3, 5)
    chkk (fiboTwo 6) (5, 8)
    chkk (superFibo 1) 1
    chkk (superFibo 2) 1
    chkk (superFibo 3) 2
    chkk (superFibo 4) 3
    chkk (superFibo 5) 5
    chkk (superFibo 6) 8
    chkk (sumFunction intSqr 1) 1
    chkk (sumFunction intSqr 4) 5
    chkk (sumFunction intSqr 5) 7

    -- 1.5
    {-chkk (hanoi 0) []-}
    {-chkk (hanoi 2) [('a', 'b'), ('a', 'c'), ('b', 'c')]-}

    -- 2.1
    chkk (isEven 1) False
    chkk (isEven 0) True
    chkk (isEven (-2)) True
    chkk (reverseList [1, 2, 3]) [3, 2, 1]
    chkk (listEvens 13 29) [28, 26, 24, 22, 20, 18, 16, 14]
    chkk (listEvens 20 31) [30, 28, 26, 24, 22, 20]
    chkk (listEvens 31 20) []
    chkk (listEvens (-29) (-22)) [-22, -24, -26, -28]
    chkk (listTables 4) [(0,0,0),(0,1,0),(0,2,0),(0,3,0),(0,4,0),(1,0,0),(1,1,1),(1,2,2),(1,3,3),(1,4,4),(2,0,0),(2,1,2),(2,2,4),(3,0,0),(3,1,3),(4,0,0),(4,1,4)]
    chkk (addPairwise [1, 2] [3, 4, 5]) [4, 6]
    chkk (addPairwise [] [1, 2, 3]) []
    chkk (addPairwise [(-7), (-8), (-9)] [10, 11, 12]) [3, 3, 3]
