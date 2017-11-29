insertAt :: a -> [a] -> Int -> [a]
insertAt e l n | n <= 1 = e:l
insertAt e (h:t) n = h : (insertAt e t (n-1))
