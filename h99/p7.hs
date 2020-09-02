--  Flatten a nested list structure.

--  data NestedList a = Elem a | List [NestedList a]
-- λ> flatten (Elem 5)
-- [5]
-- λ> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> flatten (List [])
--[]

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

