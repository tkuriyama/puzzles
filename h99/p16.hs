-- (**) Drop every N'th element from a list.

-- λ> ppdropEvery "abcdefghik" 3
-- "abdeghk"

import Data.List (foldl')

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = reverse $ snd $ foldl' f (1, []) xs
  where f (i, acc) x = if i < n then (i+1, x:acc) else (1, acc)
