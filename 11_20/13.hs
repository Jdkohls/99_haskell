-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them

-- im fairly sure i did this in 11

data Custom_tuple a = Single a | Multiple Int a
    deriving (Show)

--using 10 as a basis:
encode  :: Eq a => [a]->[(Int,a)]
encode x = foldr helper [] x
    where
        helper i [] = [(1,i)]
        helper i ((k,x):xs) 
            | i == x  = ((k+1,x):xs)
            | otherwise       = ((1,i):(k,x):xs)


encodeModified :: Eq a => [a]->[Custom_tuple a]
encodeModified = map func . encode
    where 
        func (i,x)
            | i == 1    = Single x
            | otherwise = Multiple i x