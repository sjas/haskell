--
-- Contains the test methods, to separate the logical implementation, the execution and the test code logically.
--

module Test where

-- first test method
chk :: Bool -> IO ()
chk a = if a == True 
        then returnMode okString
        else returnMode failString

-- another, more general test method
chkk :: Ord x => x -> x -> IO ()
chkk a b | a == b = returnMode okString
         | otherwise = returnMode failString



-- HELPER METHODS BELOW HERE

-- I have seen the irony of what I did after the type declarations...
okString :: String
okString = "ok "
failString :: String
failString = "### FAIL ### "

-- to easier change output design, choose one version
returnMode :: String -> IO ()
{-returnMode n = print n-}
{-returnMode n = putStr n-}
returnMode n = putStrLn n
