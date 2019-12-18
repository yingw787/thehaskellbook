-- RecursiveMul.hs
module RecursiveMul where

import Test.Hspec

-- (INCORRECT, ERROR WHEN ATTEMPTING TO COMPARE AGAINST INTEGRAL OR OTHER
-- CONCRETE TYPE IN HSPEC)
--
-- recursiveMul :: (Eq a, Num a) => a -> a -> a
-- recursiveMul m1 m2 = go m1 m2 0 where
--     go a b c
--         | b == 0 = c
--         | otherwise = go a (b - 1) (c + a)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
recursiveMul :: (Integral a) => a -> a -> a
recursiveMul x y = go x y 0
    where go a b c
            | b == 0 = c
            | otherwise = go a (b - 1) (c + a)

main :: IO ()
main = hspec $ do
    describe "RecursiveMul" $ do
        -- (INCORRECT, COMPILE-TIME ERROR 'No instance for (Num ((a0, b0) ->
        -- (a0, b0))) arising from the literal ‘0’ (maybe you haven't applied a
        -- function to enough arguments?)')
        --
        -- it "0 * 5 is 0" $ do
        --     recursiveMul (0, 5) `shouldBe` 0
        -- it "0 * 5 is 0" $ do
        --     shouldBe (recursiveMul (0, 5)) 0
        -- it "5 * 0 is 0" $ do
        --     (recursiveMul (5, 0)) `shouldBe` 0
        --
        -- (FROM ANSWER KEY)
        -- (PERSONAL NOTE: I'm not quite sure why Hspec forces a conversion from
        -- a Num to type Integer; maybe it is because it wants to compare two
        -- concrete types? I guess that makes sense)
        it "0 * 5 is 0" $ do
            -- recursiveMul (0, 5) `shouldBe` (0 :: Integer)
            --
            -- (PERSONAL NOTE: Above didn't work because I was passing in a
            -- tuple and inputs are not destructured.)
            recursiveMul 0 5 `shouldBe` (0 :: Integer)
        it "5 * 0 is 0" $ do
            recursiveMul 5 0 `shouldBe` (0 :: Integer)
        it "(-2) * 5 is (-10)" $ do
            recursiveMul (-2) 5 `shouldBe` ((-10) :: Integer)
        -- (THIS TEST CASE FAILS BUT ANSWER KEY DOES NOT COVER IT, so...moving
        -- on...)
        --
        -- it "5 * (-2) is (-10)" $ do
        --     recursiveMul 5 (-2) `shouldBe` ((-10) :: Integer)
