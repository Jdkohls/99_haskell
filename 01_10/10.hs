-- Run-length encoding of a list. 

-- Let's use 9 as a basis
encode  :: Eq a => [a]->[(Int,a)]
encode x = foldr helper [] x
    where
        helper i [] = [(1,i)]
        helper i ((k,x):xs) 
            | i == x  = ((k+1,x):xs)
            | otherwise       = ((1,i):(k,x):xs)
