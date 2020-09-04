-- Extract a given number of randomly selected elements from a list.

-- Example in Haskell:

-- λ> rnd_select "abcdefgh" 3 >>= putStrLn

import System.IO
import System.Random (randomRIO)
import Control.Monad (replicateM)

rndSelect :: [a] -> Int -> IO [a]  
rndSelect l n = do
    is <- replicateM n $ randomRIO (0, length l - 1)
    return $ map (\i -> l !! i) is
