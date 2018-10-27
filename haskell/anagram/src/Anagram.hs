module Anagram
    ( anagramsFor
    ) where

import Data.Char
import Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagram xs) xss

isAnagram :: String -> String -> Bool
isAnagram x y = lx /= ly && sort lx == sort ly
  where
    lx = map toLower x
    ly = map toLower y
