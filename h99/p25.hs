-- Generate a random permutation of the elements of a list.
-- λ> rnd_permu "abcdef"
-- "badcef"

import System.Random (randomRIO)
import Data.List (uncons)

-- a mostly functional (though not the most efficient) solution

type Partial a = [a]
type Rest a = [a]
type Pair a = (Partial a, Rest a)

factorial xs = product [1..length xs]

rotate :: [a] -> Int -> [a]
rotate xs n = drop n' xs ++ take n' xs
  where n' | n >= 0 = n
           | n < 0 = length xs + n

genPerms :: [a] -> [[a]]
genPerms xs = map fst . flip (!!) (length xs) $ iterate f p0
  where p0 = [([], xs)]
        f pair = concatMap expandPair pair
    
expandPair :: Pair a -> [Pair a]
expandPair (ps, xs) = map (\(p, xs') -> ((p:ps), xs')) (genPairs xs)
  where
    rotations :: [a] -> [[a]]
    rotations xs = map (\n -> rotate xs n) [0..length xs - 1]
    genPairs :: [a] -> [(a, [a])]
    genPairs = map f . rotations
      where f (x:xs) = (x, xs)
            f [] = error "genPairs of empty list"
               
randPerm :: [a] -> IO [a]
randPerm xs = do
    ps <- return $ genPerms xs
    is <- randomRIO (0, (factorial xs) - 1)
    return $ ps !! is
