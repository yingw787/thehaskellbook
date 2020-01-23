-- ChapterExercises.hs
--
-- (Unless otherwise stated, answers are from answer key:
-- https://github.com/johnchandlerburnham/hpfp)
module ChapterExercises where

import WriteStateForYourself


-- 1)
-- (PERSONAL NOTE: I know it's something related to method 'id'...)
get :: Moi s s
get = Moi $ \s -> (s, s)

-- 2)
put :: s -> Moi s ()
put s = Moi $ \s' -> ((), s)

-- 3)
exec :: Moi s a -> s -> s
exec (Moi sa) s = let (a, s1) = (sa s) in s1

-- 4)
eval :: Moi s a -> s -> a
eval (Moi sa) s = let (a, s1) = (sa s) in a

-- 5)
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), (f s))


main :: IO ()
main = do
    print $ runMoi get "amazing"

    print $ runMoi (put "huh") "what"

    print $ exec (put "wilma") "daphne"
    print $ exec get "scooby doo"

    print $ eval get "bunnicula"
    print $ eval get "stake a bunny"

    print $ runMoi (modify (+1)) 0
    print $ runMoi (modify (+1) >> modify (+1)) 0
