-- !! REDO LATER

--Pack consecutive duplicates of list elements into sublists. 
--pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
--["aaaa","b","cc","aa","d","eeee"]

-- helper 'a' []
-- helper 'a' ["a"]
-- so in the line foldr helper [] x
-- [] is the accumulator
-- 
-- pack ['a','a','e']
-- helper 'e' []
-- helper 'a' ["e"]
-- helper 'a' == 'e' -> "a":"e":[]
-- helper 'a' ["a","e"]
-- head is preforming str->char here.


pack :: Eq a => [a]->[[a]]
pack x = foldr helper [] x
    where
        helper i [] = [[i]]
        helper i (x:xs) 
            | i == (head x)  = ((i:x):xs)
            | otherwise       = ([i]:x:xs)

