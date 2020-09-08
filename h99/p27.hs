-- Group the elements of a set into disjoint subsets.

-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

-- Example in Haskell:

-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)

-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)

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


type Pair a = ([a], [a])

subsets :: (Eq a) => [Int] -> [a] -> [[a]]
subsets ns xs = map fst $ f ns [([], xs)]
  where
    f :: (Eq a) => [Int] -> [Pair a] -> [Pair a]
    f [] xs = xs
    f _ [] = []
    f (n:ns) xs = f ns $ concatMap (genSubsets n) xs
  
genSubsets :: (Eq a) => Int -> Pair a -> [Pair a]
genSubsets n xs@(_, []) = [xs]
genSubsets n xs@(_, rest) | n > length rest = [xs]
genSubsets n xs@(prev, rest) =
  let combs = combinations n rest
  in map (\ys -> (prev ++ ys, rest `minus` ys)) combs
  where minus as bs = [a | a <- as, a `notElem` bs]


test1 = length (subsets [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]) == 1260

test2 = length (subsets [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]) == 756

test = test1 && test2
