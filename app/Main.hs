module Main (main) where

import Control.Monad
import Data.Maybe (mapMaybe)
import System.Environment
import System.IO

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
     in do
            putStrLn $ "Linear:" ++ show linear ++ "\nLagrange:" ++ show lagrange
            interact process
  where
    process = unlines {-. map show-} . mapMaybe parsePair . lines
    parsePair line = case map (read :: String -> Float) (words line) of
        [x, y] -> do
            -- future logic with numbers here
            Just (show x ++ " " ++ show y)
        _ -> Nothing
