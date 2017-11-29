split :: [a] -> Int -> ([a], [a])
split lst n = split' ([], lst) n

split' :: ([a], [a]) -> Int -> ([a], [a])
split' acc 0 = acc
split' (f, []) _ = (f, [])
split' (f, h:t) n = split' (f ++ [h], t) (n - 1)
