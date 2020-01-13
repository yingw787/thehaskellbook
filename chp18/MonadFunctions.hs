-- MonadFunctions.hs
--
-- (PERSONAL NOTE: Just going to back away slowly here. Not ready to work on
-- these functions yet.)
module MonadFunctions where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- 1)
j :: Monad m => m (m a) -> m a
j = undefined

-- main1 :: IO ()
-- main1 = do
--     print $ j [[1, 2], [], [3]]
--     print $ j (Just (Just 1))
--     print $ j (Just Nothing)
--     print $ j Nothing

-- 2)

-- 3)

-- 4)

-- 5)

-- 6)
