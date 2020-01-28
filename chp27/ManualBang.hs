-- ManualBang.hs
{-# LANGUAGE BangPatterns #-}

module ManualBang where


doesntEval :: Bool -> Int
doesntEval b = 1

manualSeq :: Bool -> Int
manualSeq b = seq b 1

-- (!) forces case expression and WHNF
banging :: Bool -> Int
banging !b = 1


data DoesntForce = TisLazy Int String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s

data BangBang = SheShotMeDown !Int !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s
