-- -----------------------------------------------------------------------
-- ViewMat - A simple matrix visualizer
-- -----------------------------------------------------------------------

module ViewMat where

import Data.Matrix as M

-- | Clas VMat - An abstraction over different matrix implementations
class VMat m where
   getDimension :: m a -> ((Int,Int), (Int,Int))
   getValue :: Int -> Int -> m a -> a
   indexList :: m a -> [(Int,Int)]
   valueList :: m a -> [a]

   indexList m = [(x,y) | x <- [xMin..xMax], y <- [yMin..yMax]]
      where
        ((xMin,yMin),(xMax,yMax)) = getDimension m


instance VMat Matrix
   where
     getDimension m = ((1,nrows m), (1,ncols m))
     getValue i j = getElem i j
     valueList = M.toList 



