-- Reverse a list

myReverse :: [a] -> [a]
myReverse x = myReverseHelper x []
    where
        myReverseHelper :: [a] -> [a] -> [a]
        myReverseHelper [] x = x
        myReverseHelper (x:xs) m = myReverseHelper xs (x:m)