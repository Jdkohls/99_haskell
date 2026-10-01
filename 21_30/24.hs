
range :: Int -> Int -> [Int]
range i j = [i..j]

rndSelect :: [a] -> Int -> [a]
rndSelect [] _ = []

-- Lotto: Draw N different random numbers from the set 1..M. 
-- diffSelect :: N -> M -> [Int]
diffSelect :: Int -> Int -> [Int]
diffSelect = flip $ rndSelect . range 1