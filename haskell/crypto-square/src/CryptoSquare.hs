module CryptoSquare
    ( encode
    ) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose, unwords)
import Data.List.Split (chunksOf)
import Text.Printf (printf)

s = "If man was meant to stay on the ground, god would have given us roots."

encode :: String -> String
encode xs =
    let ss = splitSize xs
     in unwords . transpose . pad ss . chunksOf ss . removeSymbols $ xs

removeSymbols :: String -> String
removeSymbols = map toLower . filter isAlphaNum

splitSize :: (Integral a) => String -> a
splitSize = floor . sqrt . fromIntegral . length

pad :: Int -> [String] -> [String]
pad r = map (printf ("%-" ++ show r ++ "s"))
