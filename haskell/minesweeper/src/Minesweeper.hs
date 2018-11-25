module Minesweeper
    ( annotate
    ) where

import Data.Char
import Data.Matrix

s = fromLists [" * * ", "  *  ", "  *  ", "     "]

annotate :: [String] -> [String]
annotate board
    | length board == 0 = []
    | otherwise = toLists $ solve $ fromLists board

solve :: Matrix Char -> Matrix Char
solve m = matrix (nrows m) (ncols m) $ calc m

calc :: Matrix Char -> (Int, Int) -> Char
calc m (r, c)
    | m ! (r, c) == '*' = '*'
    | otherwise =
        case calcValue m r c of
            0 -> ' '
            d -> intToDigit d

calcValue :: Matrix Char -> Int -> Int -> Int
calcValue m r c = sum $ map (cellGet m) (indices r c)

cellGet :: Matrix Char -> (Int, Int) -> Int
cellGet m (r, c) =
    case safeGet r c m of
        Just '*' -> 1
        _ -> 0

indices :: Int -> Int -> [(Int, Int)]
indices r c =
    [(dr + r, dc + c) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
