module PerfectNumbers
    ( classify
    , Classification(..)
    ) where

import Data.List

data Classification
    = Deficient
    | Perfect
    | Abundant
    deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
    | x < 1 = Nothing
    | x == 1 || x == 2 = Just Deficient
    | s == x = Just Perfect
    | s > x = Just Abundant
    | s < x = Just Deficient
    | otherwise = Nothing
  where
    s = sum . findFactors $ x

findFactors :: Int -> [Int]
findFactors x
    | x < 1 = []
    | x < 3 = [1]
    | otherwise =
        1 :
        (nub . concat . map (findFactor x) $
         [2 .. (ceiling . sqrt . fromIntegral $ x)])

findFactor :: Int -> Int -> [Int]
findFactor x y =
    if r == 0
        then [d, y]
        else []
  where
    r = x `rem` y
    d = x `div` y
