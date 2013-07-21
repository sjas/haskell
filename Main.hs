--
-- This file contains tests for the haskell files in the same folder.
--

module Main (main) where
import EinsEins
import EinsZwei
import Test

main :: IO ()
main 
    = do

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
