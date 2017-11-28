data Duplicate a = Single a | Multiple Int a deriving Show

decodeModified :: [Duplicate a] -> [a]
decodeModified [] = []
decodeModified (h:t) = (expandDuplicate h) ++ (decodeModified t)

expandDuplicate :: Duplicate a -> [a]
expandDuplicate (Single x) = [x]
expandDuplicate (Multiple n x) = replicate n x


