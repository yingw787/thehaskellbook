-- RollYourOwn.hs
--
-- (Unless otherwise noted, code is from answer key:
-- https://github.com/johnchandlerburnham/hpfp)
module RollYourOwn where

import System.Random
import RandomExample


-- 1)
--
-- (PERSONAL NOTE: Trying to work on this one on my own, based on
-- RandomExample2.hs/rollsToGetTwenty)
--
-- (CORRECT BY GHCI OUTPUT)
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= n = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen
--
-- (ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
rollsToGetN' :: Int -> StdGen -> Int
rollsToGetN' n g = go 0 0 n g
  where
    go :: Int -> Int -> Int -> StdGen -> Int
    go sum count max gen
      | sum >= max = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) max nextGen

-- 2)
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] n g where
    go :: Int -> Int -> [Die] -> Int -> StdGen -> (Int, [Die])
    go sum count log max gen
        | sum >= max = (count, log)
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) ((intToDie die) : log) max nextGen
