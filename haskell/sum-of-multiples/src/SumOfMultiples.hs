module SumOfMultiples
    ( sumOfMultiples
    ) where

import Data.List
import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
    sum . Set.toList . Set.fromList . concat . map multiples $ factors
  where
    multiples x = takeWhile (< limit) . map (* x) $ [1 ..]
