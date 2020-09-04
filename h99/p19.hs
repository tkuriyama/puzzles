-- (**) Rotate a list N places to the left.

-- Hint: Use the predefined functions length and (++).

-- 
-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"

-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"


rotate :: [a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
  where n'
          | n >= 0 = n
          | n < 0 = length xs + n
  
