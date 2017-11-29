range :: Int -> Int -> [Int]
range = range' []

range' :: [Int] -> Int -> Int -> [Int]
range' acc lb ub
    | lb > ub = acc
    | otherwise = range' (acc ++ [lb]) (lb + 1) ub
