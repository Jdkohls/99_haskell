-- Find the last-but-one (or second-last) element of a list. 

myButLast :: [a] -> a
myButLast [] = error "List is empty"
myButLast [x] = error "List is too empty"
myButLast (x:_:[]) = x
myButLast (x:xs) = myLast xs

