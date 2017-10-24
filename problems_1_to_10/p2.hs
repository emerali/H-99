myButLast :: [a] -> a
myButLast [] = error "List must be non-empty"
myButLast (x:[]) = error "List must have at least two elements"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs
