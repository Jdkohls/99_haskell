-- Extract a given number of randomly selected elements from a list. 
import System.Random

rndSelect :: [a] -> Int -> [a]
rndSelect [] _ = []

