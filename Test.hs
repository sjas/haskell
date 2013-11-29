--
-- Contains the test functions, to separate logical implementation, executional and test code.
--

module Test where

-- first test function
chk :: Bool -> IO ()
chk a | a = returnMode okString
      | otherwise = returnMode failString

-- another, more general test function
chkk :: Eq a => a -> a -> IO ()
chkk a b | a == b = returnMode okString
         | otherwise = returnMode failString



-- HELPERS BELOW HERE

-- I have seen the irony of what I did after the type declarations...
okString :: String
okString = "ok "
failString :: String
failString = "### FAIL ### "

-- to easier change output design, choose one version
returnMode :: String -> IO ()
{-returnMode n = print n-}
-- returnMode = putStr
returnMode = putStrLn
