-- Reverse a list.

import Data.List

reverse' :: [a] -> [a]
reverse' = foldl' (\acc x -> x:acc) []

