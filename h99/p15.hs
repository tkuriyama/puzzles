-- (**) Replicate the elements of a list a given number of times.

-- λ> repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli xs n = concatMap (take n . repeat ) xs
