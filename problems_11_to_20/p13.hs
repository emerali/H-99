data Duplicate a = Single a | Multiple Int a deriving Show

encodeDirect :: Eq a => [a] -> [Duplicate a]
encodeDirect = encodeDirect' []

encodeDirect' :: Eq a => [Duplicate a] -> [a] -> [Duplicate a]
encodeDirect' acc [] = acc
encodeDirect' [] (h:t) = encodeDirect' [Single h] t
encodeDirect' acc (h:t) =
    case (last acc) of
        dup@(Multiple _ x) | h == x -> encodeDirect' ((init acc) ++ [incDupl dup]) t
        dup@(Single x) | h == x -> encodeDirect' ((init acc) ++ [incDupl dup]) t
        _ -> encodeDirect' (acc ++ [Single h]) t

incDupl :: Duplicate a -> Duplicate a
incDupl (Single x) = Multiple 2 x
incDupl (Multiple n x) = Multiple (n + 1) x
