--  Insert an element at a given position into a list.
-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"

import Data.List (foldl')

insertAt :: a -> [a] -> Int -> [a]
insertAt c xs n = reverse . snd $ foldl' f (1, []) xs
  where f (i, acc) x = (i+1, if i == n then x:c:acc else x:acc)
