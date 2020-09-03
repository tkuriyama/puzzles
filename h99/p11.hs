-- (*) Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data RunLength = Single Char | Multiple Int Char
  deriving (Eq, Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\g -> (length g, head g)) . pack

encodeModified = map f . encode
  where f (n, c) = if n > 1 then Multiple n c else Single c
