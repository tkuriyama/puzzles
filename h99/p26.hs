-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list

-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

--λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]

import Data.List (tails, sort, nub)

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter (not . null) $ genCombs n [] xs

genCombs :: Int -> [a] -> [a] -> [[a]]
genCombs _ _ [] = []
genCombs n _ _ | n < 1 = []
genCombs 1 xs ys = [y:xs | y <- ys]
genCombs n xs ys = concatMap f $ split ys
  where toPair zs = (head zs, tail zs)
        split = map toPair . filter (not . null) . tails
        f (y,ys') = genCombs (n-1) (y:xs) ys'

test = (length . nub . map sort $ combinations 3 [1..12]) == 220
