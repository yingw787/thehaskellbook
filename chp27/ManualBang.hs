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
