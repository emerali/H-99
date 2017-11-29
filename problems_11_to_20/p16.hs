dropEvery :: [a] -> Int -> [a]
dropEvery = dropEvery' 1

dropEvery' :: Int -> [a] -> Int -> [a]
dropEvery' _ [] _ = []
dropEvery' _ lst 0 = lst
dropEvery' cnt (h:t) n
    | cnt == n = dropEvery' 1 t n
    | otherwise = h : (dropEvery' (cnt + 1) t n)
