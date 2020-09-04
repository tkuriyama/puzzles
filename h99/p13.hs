
-- (**) Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- λ> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

import Data.List (group)

data RunLength a = Single a | Multiple Int a
  deriving (Eq, Show)

encodeDirect :: (Eq a) => [a] -> [RunLength a]
encodeDirect = map f . group 
  where f g@(x:_) = let l = length g
                    in if l == 1 then Single x else Multiple l x 
