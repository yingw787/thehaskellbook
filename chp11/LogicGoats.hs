{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

-- LogicGoats.hs
module LogicGoats where

class TooMany a where
    tooMany :: a -> Bool

-- 1)
--
-- class TooMany (Int, String) where
--     tooMany :: a -> Bool
--
-- class TooMany a where
--     tooMany :: (Int, String) -> Bool
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- ((Int, String) is different from (String, Int))
instance TooMany (Int, String) where
    tooMany (n, str) = (n > 42) || (length str > 42)

-- 2)
--
-- (CORRECT)
instance TooMany (Int, Int) where
    tooMany (a, b) = ((a + b) > 42)

-- 3)
--
-- (COMPILE-TIME ERROR)
-- instance TooMany (Num a, TooMany a) => (a, a) where
--     tooMany (a, b) = ((a + b) > 42)
--
-- (FROM ANSWER KEY)
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n, m) = tooMany (n + m)
