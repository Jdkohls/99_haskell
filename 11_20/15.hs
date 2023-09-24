-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli x i = concatMap (replicate i) x

-- how to pointfree this?
-- https://pointfree.io/
repli' :: [a] -> Int -> [a]
repli' = (. replicate) . (>>=)

-- okay this looks like it should be nonsense. Monads.

repli'' :: [a] -> Int -> [a]
repli'' = flip $ concatMap . replicate

--from the answer sheet. This is what i was trying to do for a while :sob:
