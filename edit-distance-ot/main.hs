module Main where

import Hedit

testDoc = do
    d1 <- return "Hi there!"
    d2 <- return "H there!"
    d3 <- return "Hi there you!"
    diff <- return $ diffDocs d1 d2
    diff2 <- return $ diffDocs d1 d3
    diff3 <- return $ rebaseEdits diff diff2
    putStrLn $ "Diff is: " ++ (show diff)
    putStrLn $ "Diff2 is: " ++ (show diff2)
    putStrLn $ "Diff3 is: " ++ (show diff3)
    d4 <- return $ applyEdits d1 diff
    d5 <- return $ applyEdits d4 diff3
    putStrLn $ "Applied d4 is: " ++ d4
    putStrLn $ "Applied d5 is: " ++ d5

main = do
    testDoc
