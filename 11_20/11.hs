-- Modified run-length encoding
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. 
-- (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))

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
encodeModified x = map (\(i,x) -> if i == 1 then Single x else Multiple i x) (encode x)