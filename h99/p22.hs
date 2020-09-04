-- Create a list containing all integers within a given range.


-- λ> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range a b = [a..b]

range' :: Int -> Int -> [Int]
range' a b = take (b-a+1) $ iterate (\n -> n+1) a
                       


import System.IO
import System.Random (randomRIO)
import Control.Monad (replicateM)

rndSelect :: [a] -> Int -> IO [a]  
rndSelect l n = do
    is <- replicateM n $ randomRIO (0, length l - 1)
    return $ map (\i -> l !! i) is
