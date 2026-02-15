import Data.Maybe
import Lib

tests :: [Bool]
tests = [testLinear1, testLinear2, testLinear3, testLagrange1, testLagrange2]

main :: IO ()
main = do
    if and tests then putStrLn "All tests passed" else error "Something failed"

-- Nothing on not enough data
testLinear1 :: Bool
testLinear1 = isNothing $ linearInterpolate 1 [1] [1]

-- Correct result on correct input
testLinear2 :: Bool
testLinear2 = linearInterpolate 2 [0, 1] [0, 1] == Just 2

-- Only last 2 points taken
testLinear3 :: Bool
testLinear3 = linearInterpolate 4 [-1, 1, 2] [-2, 1, 2] == Just 4

testLagrange1 :: Bool
testLagrange1 = lagrangeInterpolate 2 [-2, -1, 0] [4, 1, 0] == 4

testLagrange2 :: Bool
testLagrange2 = lagrangeInterpolate (-10) [-2, 0, 1] [4, 0, 1] == 100
