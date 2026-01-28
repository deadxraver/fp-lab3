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

getN :: [String] -> Maybe Integer
getN ("-n" : n : _) = Just $ read n
getN (_ : ss) = getN ss
getN [] = Nothing

getStep :: [String] -> Maybe Float
getStep ("--step" : step : _) = Just $ read step
getStep (_ : ss) = getStep ss
getStep [] = Nothing

main = do
    args <- getArgs
    let linear = checkLinear args
        lagrange = checkLagrange args
        maybeN = getN args
        maybeStep = getStep args
        process = unlines {-. map show-} . mapMaybe parsePair . lines
        parsePair line = case map (read :: String -> Float) (words line) of
            [x, y] -> do
                -- TODO: future logic with numbers here
                Just (show x ++ " " ++ show y)
            _ -> Nothing
     in do
            putStrLn $ "Linear:" ++ show linear ++ "\nLagrange:" ++ show lagrange
            case maybeN of
                Just n -> putStrLn $ "n=" ++ show n
                Nothing -> error "n not specified"
            case maybeStep of
                Just step -> putStrLn $ "step=" ++ show step
                Nothing -> error "step not specified"
            interact process
