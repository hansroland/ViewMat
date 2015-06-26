-- -----------------------------------------------------------------------
-- ViewMat - A simple matrix visualizer
-- -----------------------------------------------------------------------

module Graphics.ViewMat where

import Data.Matrix as M
import Data.Map
import Codec.Picture

-- | Define the options used to display the matrix
data ViewMatOptions = ViewMatOptions
  { maxX :: Int
  , maxY :: Int
  }

-- | Default options used normally
defaultOptions :: ViewMatOptions 
defaultOptions = ViewMatOptions { maxX = 800, maxY = 800 }

-- | Clas VMat - An abstraction over different matrix implementations
class VMat m where
   getDimension :: m a -> ((Int,Int), (Int,Int))
   getValue :: Int -> Int -> m a -> a
   toList1 :: m a -> [a]

instance VMat Matrix
   where
     getDimension m = ((1,nrows m), (1,ncols m))
     getValue i j = getElem i j
     toList1 = M.toList 


viewMat :: (Eq a, VMat m) => m a -> IO()
viewMat m = do
    return ()


-- Helper Functions
-- freqDist :: 

-- - frequency :: (Ord a) => [a] -> [(a, Int)]
-- - frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

