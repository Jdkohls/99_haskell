--Eliminate consecutive duplicates of list elements

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = reverse (helper xs x [x])
    where
        helper :: (Eq a) => [a] -> a -> [a] -> [a]
        helper [] x acc = acc
        helper (xi:xs) x acc 
            | xi == x   = helper xs x acc 
            | otherwise =  helper xs xi (xi:acc)