-- Create a list containing all integers within a given range.


-- λ> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b = take (b-a+1) $ iterate (\n -> n+1) a
                       
