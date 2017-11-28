myLast :: [a] -> a
myLast [] = error "List must be non-empty"
myLast (x:[]) = x
myLast (x:xs) = myLast xs
