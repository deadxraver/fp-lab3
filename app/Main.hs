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

getN :: [String] -> Maybe Int
getN ("-n" : n : _) = Just $ read n
getN (_ : ss) = getN ss
getN [] = Nothing

getStep :: [String] -> Maybe Double
getStep ("--step" : step : _) = Just $ read step
getStep (_ : ss) = getStep ss
getStep [] = Nothing

valsBetween x step lower upper
    | x >= upper = []
    | x < lower = valsBetween (x + step) step lower upper
    | otherwise = x : valsBetween (x + step) step lower upper

calcToBound [] _ _ = return ()
calcToBound (x : xarr) method sign = do
    case method x of
        Just y -> putStrLn $ sign ++ show x ++ " " ++ show y
        Nothing -> return ()
    calcToBound xarr method sign

main = do
    args <- getArgs
    let linear = checkLinear args
        lagrange = checkLagrange args
        maybeN = getN args
        maybeStep = getStep args
        parsePair line = case map (read :: String -> Double) (words line) of
            [x, y] -> do
                Just (x, y)
            _ -> Nothing
        loop xLi xLa xarr yarr = do
            eof <- isEOF
            if eof
                then do
                    when linear $ putStrLn $ "linear: " ++ show (head xarr) ++ " " ++ show (head yarr)
                    when lagrange $ putStrLn $ "lagrange: " ++ show (head xarr) ++ " " ++ show (head yarr)
                else do
                    line <- getLine
                    case parsePair line of
                        Just (xi, yi) -> do
                            let newXarr = xi : xarr
                                newYarr = yi : yarr
                                step = fromJust maybeStep
                                n = fromJust maybeN
                                calcArrLi = valsBetween xLi step (head xarr) xi
                                calcArrLa = valsBetween xLa step xLa xi
                                nextXLi = case calcArrLi of
                                    (val : _) -> val
                                    [] -> xLi
                                nextXLa =
                                    if length newXarr >= n
                                        then head newXarr
                                        else xLa
                            when (not (null xarr) && xi < head xarr) $
                                error "x input should be sorted!"
                            when linear $
                                calcToBound calcArrLi (\newX -> linearInterpolate newX newXarr newYarr) "linear: "
                            when lagrange $
                                calcToBound calcArrLa (\newX -> lagrangeInterpolateN newX newXarr newYarr n) "lagrange: "
                            loop nextXLi nextXLa newXarr newXarr
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
    eof <- isEOF
    unless eof $ do
        line <- getLine
        case parsePair line of
            Just (x, y) -> loop x x [x] [y]
            Nothing -> error $ "unknown format: " ++ line
