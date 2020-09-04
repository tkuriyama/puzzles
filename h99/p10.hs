-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

data RunLength a = Single a | Multiple Int a
  deriving (Eq, Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\g -> (length g, head g)) . pack

encodeModified = map f . encode
  where f (n, c) = if n > 1 then Multiple n c else Single c


encode :: (Eq a) => [a] -> [(Int, a)]
encode = foldr f (0, '', [])
  where
    f c (n, prev, acc)
      | c /= prev && n 
