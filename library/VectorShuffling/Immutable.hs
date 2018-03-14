module VectorShuffling.Immutable
where

import           VectorShuffling.Prelude
import           Data.Vector
import qualified VectorShuffling.Mutable as B
import qualified System.Random           as A


shuffle :: Vector a -> A.StdGen -> (Vector a, A.StdGen)
shuffle vector gen =
  runST $ do
    mutable <- thaw vector
    gen'    <- B.shuffle mutable gen
    frozen  <- unsafeFreeze mutable
    return (frozen, gen')
