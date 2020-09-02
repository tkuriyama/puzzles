
-- Find the last element of a list.

myLast :: [a] -> a
myLast = foldl (\a x -> x) undefined 
