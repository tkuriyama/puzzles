-- (*) Remove the K'th element from a list.
-- λ> removeAt 2 "abcd"
-- ('b',"acd")

import Data.List (foldl')

removeAt :: Int -> [a] -> ([a], [a])
removeAt n = (\(xs, ys) -> (xs, reverse ys)). snd . foldl' f (1, ([], []))
  where f (i, (xs, ys)) x =
          (i+1, if i == n then (x:xs, ys) else (xs, x:ys))
