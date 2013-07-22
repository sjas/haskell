--
-- This file contains tests for the haskell files in the same folder.
--

module Main (main) where
import EinsEins
import EinsZwei
import EinsDrei
import Test

main :: IO ()
main 
    = do 

    print "Starting Tests..."

    -- 1.1
    {-chk(double 2 == 4)-}
    {-chk(ratio 2.5 1.5 == 4.0)-}
    {-chk(hyp 3 4 == 5)-}
    {-chk(xIntercept 0.5 6 3 == -6)-}
    {-chk(threediff 1 2 3 == True)-}
    {-chk(averageThree 3 4 10 == 5.6666665)-}
    {-chk(arithmeticSum 2 3 4 == 18)-}
    {-chk(inRange1 1 2 3 == False)-}
    {-chk(inRange1 2 1 3 == True)-}
    {-chk(inRange1 2 3 1 == True)-}
    {-chk(orExclusive True False == True)-}
    {-chk(orExclusive True True == False)-}
    {-chk(orExclusive False False == False)-}
    {-chk(implies True False == False)-}
    {-chk(implies False True == True)-}
    {-chk(implies True True == True)-}
    {-chk(implies False False == True)-}
    {-chk(hundreds 1234 == 2)-}
    {-chk(hundreds 24 == 0)-}
    {-chk(hundreds 321 == 3)-}
    {-chk(isSmall 'a' == True)-}
    {-chk(isSmall 'B' == False)-}
    {-chk(upperCase 'a' == 'A')-}
    {-chk(upperCase 'B' == 'B')-}
    {-chk(arrayLength "asdf" == 4)-}
    {-chk(arrayLength "" == 0)-}
    {-chk(arrayLength "1234567890" == 10)-}
{--- after I succeeded with a better test method implementation, ofc the new one will be used-}
    {-chkk (arrayLength "asdf") 4-}
    {-chkk (arrayLength "") 0-}
    {-chkk (arrayLength "1234567890") 10-}
    {--- test of internal function 'length'-}
    {-chkk (length "1234567890") 10-}
    {-chkk (middleIdx "asdfg") 2-}
    {-chkk (middle "wxAyz") 'A'-}
    {-chkk (middle "wxBy") 'B'-}
    {-chkk (substring "asdfaXYZa" "XYZ") True-}
    {-chkk (substring "asdfaXZa" "XYZ") False-}

    -- 1.2
    {-chkk (convertSecToMin 80) (1,20)-}
    {-chkk (convertMinToSec (1,20)) 80-}
    {-chkk (convertMinToSec (convertSecToMin 3333)) 3333-}
    {-chkk (sumUpTimeHelper [(12,59)]) (convertMinToSec (12,59))-}
    {-chkk (sumUpTime [(5,18), (3, 27), (3, 25)]) (12, 10)-}

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
