-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
--  λ> split "abcdefghik" 3
-- ("abc", "defghik")

import Data.List (foldl')

split :: [a] -> Int -> ([a], [a])
split xs n = (\(a, b) -> (reverse a, reverse b)) . snd $
             foldl f (0, ([], [])) xs
  where
    f (i, (xs, xs')) x
      | i < n = (i+1, (x:xs, xs'))
      | otherwise = (i, (xs, x:xs'))
