-- (**) Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

-- λ> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

data RunLength a = Single a | Multiple Int a
  deriving (Eq, Show)

-- p11

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : (pack $ dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\g -> (length g, head g)) . pack

encodeModified :: (Eq a) => [a] -> [RunLength a]
encodeModified = map f . encode
  where f (n, c) = if n > 1 then Multiple n c else Single c

-- p12

decodeModified :: (Eq a) => [RunLength a] -> [a]
decodeModified = concatMap decode 
  where decode (Single c) = [c]
        decode (Multiple n c) = take n $ repeat c
