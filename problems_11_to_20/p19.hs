rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate (h:t) n
    | n < 0 = rotate (h:t) ((length t) + 1 + n)
    | n == 0 = (h:t)
    | otherwise = rotate (t ++ [h]) (n-1)
