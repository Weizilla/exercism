module Luhn
    ( isValid
    ) where

import Data.Char
import Data.List

s = "4539 1488 0343 6467"

isValid :: String -> Bool
isValid n
    | length digits > 1 = sumDigits digits `mod` 10 == 0
    | otherwise = False
  where
    digits = filter isNumber n

sumDigits :: String -> Int
sumDigits x =
    sum $ zipWith ($) (cycle [id, luhnDigit]) (reverse $ map digitToInt x)

luhnDigit :: Int -> Int
luhnDigit n =
    n * 2 -
    if n < 5
        then 0
        else 9
