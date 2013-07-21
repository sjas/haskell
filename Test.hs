--
-- Contains just the single test method, to separate the code logically.
--

module Test where

-- TEST METHOD
-- lacks type definition because I don't know how to 'type' it yet.
-- thus letting haskell inferring it.
chk a = if a == True 
        then print "OK"
        else print "----- TEST FAIL -----"
