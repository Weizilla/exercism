module LeapYear
    ( isLeapYear
    ) where

isLeapYear :: Integer -> Bool
isLeapYear year =
    year `isDivisibleBy` 4 &&
    ((not $ year `isDivisibleBy` 100) || (year `isDivisibleBy` 400))

isLeapYear' :: Integer -> Bool
isLeapYear' y
    | y `isDivisibleBy` 400 = True
    | y `isDivisibleBy` 100 = False
    | y `isDivisibleBy` 4 = True
    | otherwise = False

isDivisibleBy :: Integer -> Integer -> Bool
x `isDivisibleBy` y = x `rem` y == 0
