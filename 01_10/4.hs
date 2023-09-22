-- Find the number of elements in a list

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength2 :: [a] -> Int
myLength2 x = myLength2Helper x 0
    where
        myLength2Helper [] i = i
        myLength2Helper (_:xs) i = myLength2Helper xs (i+1)