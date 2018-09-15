module Bob
    ( responseFor
    ) where

import Data.Char

responseFor :: String -> String
responseFor xs
    | shoutingQuestion = "Calm down, I know what I'm doing!"
    | shouting = "Whoa, chill out!"
    | silence = "Fine. Be that way!"
    | question == '?' = "Sure."
    | otherwise = "Whatever."
  where
    shouting = (not $ null $ filter isAlpha xs) && (all isUpper $ filter isAlpha $ xs)
    shoutingQuestion = shouting && (last xs == '?')
    question = last (filter (not . isSpace) xs)
    silence = all isSpace xs
