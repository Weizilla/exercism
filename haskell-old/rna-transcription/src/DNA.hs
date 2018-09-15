module DNA
    ( toRNA
    ) where

toRNA :: String -> Maybe String
toRNA xs = sequence $ map toRNA' xs
  where
    toRNA' x =
        case x of
            'G' -> Just 'C'
            'C' -> Just 'G'
            'T' -> Just 'A'
            'A' -> Just 'U'
            _ -> Nothing
