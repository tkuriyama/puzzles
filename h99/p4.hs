-- Find the number of elements of a list.

length' :: [a] -> Int 
length' = foldr (const (+1)) 0

