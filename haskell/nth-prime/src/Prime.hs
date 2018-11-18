module Prime
    ( nth
    ) where

nth :: Int -> Maybe Integer
nth n
    | n > 0 = Just $ (filter prime [1 ..]) !! (n - 1)
    | otherwise = Nothing

prime :: Integer -> Bool
prime 1 = False
prime x =
    let s = floor . sqrt $ fromIntegral x
     in all (\f -> x `mod` f /= 0) [2 .. s]
