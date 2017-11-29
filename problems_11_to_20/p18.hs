slice :: [a] -> Int -> Int -> [a]
slice = slice' 1

slice' :: Int -> [a] -> Int -> Int -> [a]
slice' _ [] _ _ = []
slice' i (h:t) lb ub
    | lb <= i && i <= ub = h : (slice' (i + 1) t lb ub)
    | otherwise = (slice' (i + 1) t lb ub)
