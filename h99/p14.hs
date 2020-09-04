-- (*) Duplicate the elements of a list.

--λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli xs = interleave xs xs

interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x : y : (interleave xs ys)
