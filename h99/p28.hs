-- Problem 28
-- Sorting a list of lists according to length of sublists

-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

-- Example:

-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]

-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

-- Example in Haskell:

-- λ> lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
-- ["ijkl","o","abc","fgh","de","de","mn"]

import qualified Data.Map as M

import Data.Function (on)
import Data.List (sort, sortBy)


lsort :: (Ord a) => [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

lfsort :: (Ord a) => [[a]] -> [[a]]
lfsort xs = map fst . sortBy (compare `on` snd) $ zip xs (lfreq xs)


lfreq :: (Ord a) => [[a]] -> [Integer]
lfreq xs = map (\x -> M.findWithDefault 0 (length x) freqMap) xs
  where pairs = zip (map length xs) (repeat 1)
        freqMap = M.fromListWith (+) pairs
