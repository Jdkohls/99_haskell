-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++). 

rotate :: [a] -> Int -> [a]
rotate [] _      = []
rotate xp@(x:xs) i  
    | i == 1  = xs ++ [x]
    | i == -1 = (last xs) : x : (init xs)
    | i < 0   = rotate (rotate xp -1) (i+1)
    | i > 0   = rotate (rotate xp 1) (i-1)
