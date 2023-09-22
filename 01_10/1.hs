-- Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "List is empty"
myLast [x] = x
myLast (_:xs) = myLast xs

