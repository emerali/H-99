removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (h:t) = (h, t)
removeAt n (h:t) =
    let (removed, restOfList) = removeAt (n-1) t
    in (removed, h:restOfList)
