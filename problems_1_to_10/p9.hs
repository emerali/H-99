pack :: Eq a => [a] -> [[a]]
pack lst = pack' [] lst

pack' :: Eq a => [[a]] -> [a] -> [[a]]
pack' acc [] = acc
pack' [] (h:t) = pack' [[h]] t
pack' acc (h:t)
    | h == (head (last acc)) = pack' ((init acc) ++ [h:(last acc)]) t
    | otherwise = pack' (acc ++ [[h]]) t
