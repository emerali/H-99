repli :: [a] -> Int -> [a]
repli lst n = repli' lst n n

repli' :: [a] -> Int -> Int -> [a]
repli' [] _ _ = []
repli' (h:t) 0 n = repli t n
repli' (h:t) k n = h : (repli' (h:t) (k - 1) n)
