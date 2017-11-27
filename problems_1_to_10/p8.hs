compress :: Eq a => [a] -> [a]
compress lst =
    case lst of
        [] -> []
        h1:h2:t | h1 == h2 -> compress (h2:t)
        h:t -> h:(compress t)
