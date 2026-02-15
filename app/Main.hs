module Main (main) where

import Control.Monad
import Data.Maybe (fromJust, mapMaybe)
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

{-main = do
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
                Nothing -> if lagrange then error "n not specified" else putStrLn "n=null"
            case maybeStep of
                Just step -> putStrLn $ "step=" ++ show step
                Nothing -> error "step not specified"
            if not (linear || lagrange)
                then error "You must pick at least 1 method (linear/lagrange)"
                else interact process
                  -}

main = do
    args <- getArgs
    let linear = checkLinear args
        lagrange = checkLagrange args
        maybeN = getN args
        maybeStep = getStep args
        parsePair line = case map (read :: String -> Float) (words line) of
            [x, y] -> do
                Just (x, y)
            _ -> Nothing
        loop x xarr yarr = do
            eof <- isEOF
            if eof
                then return ()
                else do
                    line <- getLine
                    case parsePair line of
                        Just (xi, yi) -> do
                            let newXarr = xi : xarr
                                newYarr = yi : yarr
                                step = fromJust maybeStep
                            -- TODO:
                            putStrLn $ "x=" ++ show x ++ ",X=" ++ show newXarr ++ ",Y=" ++ show newYarr
                            loop (x + step) newXarr newXarr
                        Nothing -> error $ "unknown format:" ++ line
    case maybeStep of
        Just step -> putStrLn $ "step=" ++ show step
        Nothing -> error "step not specified"
    when lagrange $
        case maybeN of
            Just n -> putStrLn $ "n=" ++ show n
            Nothing -> error "n not specified for Lagrange"
    unless (lagrange || linear) $
        error "at least 1 method should be picked"
    loop 0 [] []
