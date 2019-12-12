-- UnderstandingFolds.hs
module UnderstandingFolds where

main :: IO ()
main = do
    -- a) `foldr (++) ["woot", "WOOT", "woot"]`
    --
    -- Missing identity value. Add `""`.
    --
    -- (CORRECT)
    print $ foldr (++) "" ["woot", "WOOT", "woot"]

    -- b) `foldr max [] "fear is the little death"`
    --
    -- Incorrect input argument type String. Should be [String].
    --
    -- (INCORRECT, ANSWER KEY https://github.com/johnchandlerburnham/hpfp)
    -- (Keep the input argument type, and change the type of the identity value
    -- from `[]` to `"a"`.)
    --
    -- print $ foldr max [] ["fear", "is", "the", "little", "death"]
    print $ foldr max 'a' "fear is the little death"

    -- c) `foldr and True [False, True]`
    --
    -- Method `and` is a prefix operator, should be infixed with backticks.
    --
    -- (INCORRECT, ANSWER KEY https://github.com/johnchandlerburnham/hpfp)
    -- print $ foldr `and` True [False, True]
    -- (Might be something with parenthesization, since `and` apparently)
    --
    -- (Apparently it is an issue with prefix operator and infix operator.
    -- Apparently backticks do not work around 'and', must replace with (&&).)
    print $ foldr (&&) True [False, True]

    -- d) `foldr (||) True [False, True]`
    --
    -- Identity value for operator `(||)` should be `False`, not `True`, in
    -- order for all falsy values to return False correctly.
    print $ foldr (||) False [False, True]
