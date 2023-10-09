-- Remove the K'th element from a list. 

removeAt :: Int -> [a] -> (a,[a])
removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = removeAt' (n-1) (x:xs) []
    where
        removeAt' 0 (x:xs) p = (x, reverse p ++ xs)
        removeAt' i (x:xs) p = removeAt' (i-1) xs (x:p)