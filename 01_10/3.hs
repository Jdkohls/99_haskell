-- Find the K'th element of a list

elementAt :: [a] -> Int -> a
elementAt x 1 = head x
elementAt x i = elementAt (tail x) (i - 1)

elementAt2 :: [a] -> Int -> a
elementAt2 (x:_) 1 = x
elementAt2 (_:xs) i 
    | i <=0     = error "invalid index"
    | otherwise = elementAt2 xs (i - 1)