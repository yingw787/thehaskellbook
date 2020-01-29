-- IORefTrans.hs
module IORefTrans where

import Control.Monad (replicateM)
import System.Random (randomRIO)


gimmeShelter :: Bool -> IO [Int]
gimmeShelter True = replicateM 10 (randomIO (0, 10))
gimmeShelter False = pure [0]
