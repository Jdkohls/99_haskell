-- Flatten a nested list structure

-- need to define a new data structure
-- this datastructure was stolen from the site.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten x = flatten' x []
    where
        flatten' :: NestedList a -> [a] -> [a]
        flatten' (Elem x) i       = x:i
        flatten' (List (x:xs)) i  = flatten x ++ flatten (List xs) ++ i
        flatten' (List []) i      = i