-- Lotto: Draw N different random numbers from the set 1..M.
-- λ> diff_select 6 49
-- [23,1,17,33,21,37]

import System.IO
import System.Random (randomRIO)
import Control.Monad (replicateM)

lotto :: Int -> Int -> IO [Int]  
lotto n m = do
    is <- replicateM n $ randomRIO (0, m)
    ms <- return [0..m] 
    return $ map (\i -> ms !! i) is
