module Main (main) where

import System.Environment

import Lib

check :: String -> [String] -> Bool
check y (x : xs) = (x == y) || check y xs
check _ [] = False

checkLinear :: [String] -> Bool
checkLinear = check "--linear"

checkLagrange :: [String] -> Bool
checkLagrange = check "--lagrange"

main = do
    args <- getArgs
    let linear = checkLinear args
        lagrange = checkLagrange args
     in putStrLn $ "Linear:" ++ show linear ++ "\nLagrange:" ++ show lagrange
