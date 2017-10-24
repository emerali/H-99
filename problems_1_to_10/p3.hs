elementAt :: (Integral n) => [a] -> n -> a
elementAt [] n = error "Not enough elements in List"
elementAt (x:xs) n
    | n < 0  = error "n must be non-negative"
    | n == 0 = x
    | otherwise = elementAt xs (n - 1)
