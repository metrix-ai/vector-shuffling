module VectorShuffling.Mutable
where

import           VectorShuffling.Prelude hiding (swap, length)
import           Data.Vector.Mutable
import qualified System.Random           as A


shuffle :: MVector s a -> A.StdGen -> ST s A.StdGen
shuffle v gen =
  loop gen (length v)
  where
    loop !gen !i =
      if i <= 1
        then return gen
        else
          let
            (index, gen') = A.randomR (0, i') gen
            i' = i - 1
            in do
              swap v i' index
              loop gen' i'
