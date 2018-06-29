module RandomLibrary where
import System.Random
import Control.Monad

randomIntList :: Int -> Int -> IO([Int])
randomIntList len lim = replicateM len $ randomRIO (1,lim)

-- randomValues :: [String] -> Int -> [(String,Int)]
-- randomValues xs lim = zip xs (randomIntList (length xs) lim) 