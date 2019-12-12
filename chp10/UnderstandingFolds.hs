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

    -- e) `foldl ((++) . show) "" [1..5]`
    --
    -- Due to left associativity of foldl, `flip` should be applied in order to
    -- correctly apply method `show` to values from input argument rather than
    -- acculumator (which is already proper type).
    --
    -- Method `flip` should wrap around entire method, cannot be put in
    -- pointfree style.
    --
    -- (ANSWER KEY https://github.com/johnchandlerburnham/hpfp CHANGES `foldl`
    -- to `foldr`, not sure whether that's the right answer, but it does result
    -- in a valid result (though different from the one here).)
    print $ foldl (flip ((++) . show)) "" [1..5]

    -- f) `foldr const 'a' [1..5]`
    --
    -- Should be (const . show) in order to cast values of typeclass `Num` to
    -- Char. Otherwise, 'a' should be changed to 0 (any integer value should
    -- work with `foldr` to return the first value of the list.)
    --
    -- (ANSWER KEY https://github.com/johnchandlerburnham/hpfp changes `foldl`
    -- to `foldr` to return a result of 'a'.)
    print $ foldr const 0 [1..5]

    -- g) `foldr const 0 "tacos"`
    --
    -- Change `foldr` to `foldl`.
    --
    -- (CORRECT, MATCHES ANSWER KEY)
    print $ foldl const 0 "tacos"

    -- h) `foldl (flip const) 0 "burritos"`
    --
    -- Change `foldl` to `foldr`.
    --
    -- (CORRECT, MATCHES ANSWER KEY)
    --
    -- (I was uncomfortable with this answer originally, but after some
    -- reflection this may be the correct answer. Since `const` is lazy, the
    -- final answer should not care about casting between types.)
    print $ foldr (flip const) 0 "burritos"

    -- i) `foldl (flip const) 'z' [1..5]`
    --
    -- Change `foldl` to `foldr`.
    --
    -- (CORRECT, MATCHES ANSWER KEY)
    --
    print $ foldr (flip const) 'z' [1..5]
