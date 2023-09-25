-- Extract a slice from a list

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice p@(x:xs) i k
    | i > k     = error "crying"
    | i > 1     = slice xs (i-1) (k-1)
    | i == 1    = take k p

-- **** lisp for making us index at 1.