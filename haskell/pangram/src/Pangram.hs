module Pangram
    ( isPangram
    ) where

import Data.Char
import Data.List

isPangram :: String -> Bool
isPangram text =
    let uniqueAlphaOnly = nub [toLower x | x <- text, isAlpha x]
     in length uniqueAlphaOnly == 26
