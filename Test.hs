--
-- Contains the test methods, to separate the logical implementation, the execution and the test code logically.
--

module Test where

-- first test method
chk :: Bool -> IO ()
chk a = if a == True 
        then print "OK"
        else print "--- TEST FAIL ---"

-- another, more general test method
chkk :: Ord x => x -> x -> IO ()
chkk a b | a == b = print "OK"
        | otherwise = print "--- TEST FAIL ---"
