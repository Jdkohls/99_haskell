-- Drop every N'th element from a list. 

dropEvery :: [a] -> Int -> [a]
dropEvery xs i = (take (i-1)) xs ++ dropEvery ((drop i) xs) i