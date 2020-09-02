
-- (*) Find the last but one element of a list.

myLast2 :: [a] -> a
myLast2 = fst . foldl (\(prev', prev) x -> (prev, x)) (undefined, undefined) 

 -- 1,2,3,4
  -- (undefined 1) (2, 3) (3, 4) (4, 5) -> fst
