module CollatzConjecture
    ( collatz
    ) where

collatz :: Integer -> Maybe Integer
collatz a
    | a > 0 = Just . fromIntegral . length . takeWhile (> 1) $ iterate collatz' a
    | otherwise = Nothing
  where
    collatz' a =
        if even a
            then a `div` 2
            else 3 * a + 1
