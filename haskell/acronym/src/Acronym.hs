module Acronym
    ( abbreviate
    ) where

import Data.Char
import Data.List.Split

abbreviate :: String -> String
abbreviate [] = []
abbreviate xs = concat $ map abbreviate' $ splitOneOf "., -" xs

abbreviate' :: String -> String
abbreviate' "" = []
abbreviate' ww@(w:ws)
    | all isUpper ww = [w]
    | otherwise = (toUpper $ w) : [l | l <- ws, isUpper l]
