module Clock
    ( addDelta
    , fromHourMin
    , toString
    ) where

import Text.Printf

data Clock = Clock
    { hour :: Int
    , minute :: Int
    } deriving (Show, Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock {hour = h, minute = m}
  where
    h = mod (hour + div minute 60) 24
    m = mod minute 60

toString :: Clock -> String
toString Clock {hour = h, minute = m} = printf "%0.2d:%0.2d" h m

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute Clock {hour = h, minute = m} =
    fromHourMin (hour + h) (minute + m)
