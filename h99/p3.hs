
-- (*) Find the K'th element of a list. The first element in the list is number 1.

kth :: Int -> [a] -> Maybe a
kth _ [] = Nothing
kth n xs
  |  n < 1 = Nothing
  | otherwise = case drop (n-1) xs of
                  [] -> Nothing
                  xs' -> Just $ head xs'
