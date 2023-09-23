-- Decode a run-length encoded list.

-- This will build off of problem 11

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

decodeModified :: [Custom_tuple a] -> [a]
decodeModified = concatMap func
    where
        func :: Custom_tuple a -> [a]
        func (Single a) = [a]
        func (Multiple i a) = replicate i a
--decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

identity :: Eq a => [a] -> [a]
identity = decodeModified . encodeModified