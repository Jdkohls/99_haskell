-- Insert an element at a given position into a list. 

insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x xs 0 = x:xs
insertAt x xs i = head xs : insertAt x (tail xs) (i-1)