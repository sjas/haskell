--
-- Contains the test methods, to separate the logical implementation, the execution and the test code logically.
--

module Test where

-- first test method
chk :: Bool -> IO ()
chk a = if a == True 
        then returnMode "ok "
        else returnMode "--- TEST FAIL --- "

-- another, more general test method
chkk :: Ord x => x -> x -> IO ()
chkk a b | a == b = returnMode "ok "
        | otherwise = returnMode "--- TEST FAIL --- "

-- to easier change output design, choose one version
returnMode :: String -> IO ()
{-returnMode n = print n-}
returnMode n = putStr n
{-returnMode n = putStrLn n-}
