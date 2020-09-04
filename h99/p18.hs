--(**) Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

-- λ> oslice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

import Data.List (foldl')

slice :: [a] -> Int -> Int -> [a]
slice xs a b = reverse . snd $ foldl' f (1, []) xs
  where f (i, acc) x = (i+1, if i >= a && i <= b then x:acc else acc)
