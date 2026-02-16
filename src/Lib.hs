module Lib (
    linearInterpolate,
    lagrangeInterpolate,
    lagrangeInterpolateN,
) where

-- linear interpolation takes two first numbers if present and interpolates
--                   x          X           Y           res
linearInterpolate :: Double -> [Double] -> [Double] -> Maybe Double
linearInterpolate x (x2 : x1 : _) (y2 : y1 : _) = Just $ y2 + (y1 - y2) / (x1 - x2) * (x - x2)
linearInterpolate _ _ _ = Nothing

--                     x          X           Y           res
lagrangeInterpolate :: Double -> [Double] -> [Double] -> Double
lagrangeInterpolate _ _ [] = 0
lagrangeInterpolate x xarr (y1 : yarr) = y1 * lagrangeInterpolate' xarr + lagrangeInterpolate x xarr yarr
  where
    x1i = xarr !! (length xarr - length yarr - 1)
    lagrangeInterpolate' [] = 1
    lagrangeInterpolate' (x1' : xarr') = (if x1' == x1i then 1 else (x - x1') / (x1i - x1')) * lagrangeInterpolate' xarr'

--                      x          X           Y          n      res
lagrangeInterpolateN :: Double -> [Double] -> [Double] -> Int -> Maybe Double
lagrangeInterpolateN x xarr yarr n =
    if length xarr < n
        then Nothing
        else Just $ lagrangeInterpolate x (take n xarr) (take n yarr)
