module Test where

{-chk :: a -> a -> IO()-}
chk a = if a == True 
    then print "OK"
    else print "----- TEST FAIL -----"
