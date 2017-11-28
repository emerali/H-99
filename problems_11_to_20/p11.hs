data Duplicate a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [Duplicate a]
encodeModified = (map dupesFromList) . pack

dupesFromList :: [a] -> Duplicate a
dupesFromList x
    | l == 1 = Single (head x)
    | otherwise = Multiple l (head x)
    where l = length x

pack :: Eq a => [a] -> [[a]]
pack = pack' []

pack' :: Eq a => [[a]] -> [a] -> [[a]]
pack' acc [] = acc
pack' [] (h:t) = pack' [[h]] t
pack' acc (h:t)
    | h == (head (last acc)) = pack' ((init acc) ++ [h:(last acc)]) t
    | otherwise = pack' (acc ++ [[h]]) t
