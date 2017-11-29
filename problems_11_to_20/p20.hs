removeAt :: Int -> [a] -> (a, [a])
removeAt n l = (l !! (n-1), removeAt' n l)

removeAt' :: Int -> [a] -> [a]
removeAt' _ [] = []
removeAt' 1 (h:t) = t
removeAt' n (h:t) = h : (removeAt' (n-1) t)
