-- (**) Eliminate consecutive duplicates of list elements.

-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

-- λ> compress "aaaabccaadeeee"
-- "abcade"
import Data.List (group)

compress :: (Eq a) => [a] -> [a]
compress xs = snd . foldr f (x', [x']) $ xs'
  where (x', xs') = (last xs, init xs)
        f x (prev, xs) = if x == prev then (prev, xs) else (x, x:xs)

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group
