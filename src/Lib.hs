module Lib (
    linearInterpolate,
    lagrangeInterpolate,
) where

linearInterpolate :: Double -> [(Double, Double)] -> Double
linearInterpolate _ _ = 0 -- TODO:

--                     x          X           Y           res
lagrangeInterpolate :: Double -> [Double] -> [Double] -> Double
lagrangeInterpolate _ _ [] = 0
lagrangeInterpolate x xarr (y1 : yarr) = y1 * lagrangeInterpolate' xarr + lagrangeInterpolate x xarr yarr
  where
    x1i = xarr !! (length xarr - length yarr - 1)
    lagrangeInterpolate' [] = 1
    lagrangeInterpolate' (x1' : xarr') = (if x1' == x1i then 1 else (x - x1') / (x1i - x1')) * lagrangeInterpolate' xarr'
