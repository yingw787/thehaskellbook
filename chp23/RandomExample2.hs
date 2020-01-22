-- RandomExample2.hs
module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample


-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

-- lift `intToDie` over `state`.
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- repeat :: a -> [a]

-- Broken, repeats a single die value, doesn't repeat state that produces a die.
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= 20 = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen
