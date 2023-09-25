-- Split a list into two parts; the length of the first part is given
-- Do not use any predefined predicates

split :: [a] -> Int -> ([a],[a])
split x i = split' x [] i
    where
        split' :: [a] -> [a] -> Int -> ([a],[a])
        split' xs acc 0 = (reverse acc,xs)
        split' (x:xs) acc n = split' xs (x:acc) (n-1)
        split' [] acc n = (reverse acc, [])
        