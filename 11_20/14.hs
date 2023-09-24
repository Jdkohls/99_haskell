-- Duplicate the elements of a list. 

-- getting used to foldr/foldl

dupli :: [a] -> [a]
dupli = foldr func [] 
    where
        func i acc = i:i:acc

-- or

dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x:x:dupli' xs