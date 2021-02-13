-- Generalized solver for 538's CrossProduct puzzles.
-- e.g. https://github.com/tkuriyama/puzzles/blob/master/538/cross_product_20210212.hs

import           Data.List

type RowSize = Int
type RowProduct = Int
type ColProduct = Int

type Cell = [Int]
type Row a = [a]
type Col a = [a]
type Matrix a = [[a]]

digits = [0..9]

--------------------------------------------------------------------------------

solve :: [RowProduct] -> RowSize -> [ColProduct] -> [Matrix Int]
solve rps rn cps =
  filter f $ matrixCombs $ colCombs cps $ rowCombs rps rn
  where
    f m = all (\(a, b) -> a == b) $ zip (map product m) rps

-- possible list of numbers for each row
rowCombs :: [RowProduct] -> RowSize -> Matrix Int
rowCombs ps n = map (nub . sort . concat) $ map f ps
  where
    combs = sequence $ replicate n digits
    f p = filter (\xs -> product xs == p) combs

-- possible column combinations for each column
colCombs :: [ColProduct] -> Matrix Int -> Matrix Cell
colCombs ps xss = map f ps
  where
    combs = sequence xss
    f p = filter (\xs -> product xs == p) combs

-- generate possible matrices from column-oriented lists
matrixCombs :: Matrix Cell -> [Matrix Int]
matrixCombs ms = map transpose $ sequence ms
