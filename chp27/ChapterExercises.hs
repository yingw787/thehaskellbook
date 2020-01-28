-- ChapterExercises.hs
module ChapterExercises where


-- What will `:sprint` output?

-- 1)
--
-- ':sprint x' prints `_`, likely because '1' has not been resolved to a
-- concrete type.
--
-- (CORRECT)
let x = 1

-- 2)
--
-- ':sprint x' prints `['1']`, because `[Char]` is a concrete type.
--
-- (PRETTY MUCH CORRECT, x = "1", since `String` = `[Char]`)
let x = ['1']

-- 3)
--
-- `:sprint x` prints `_`
--
-- (CORRECT)
let x = [1]

-- 4)
--
-- `:sprint x`: prints `_`
--
-- (INCORRECT, `:sprint x = 1`, not sure why GHCi wasn't doing this earlier)
let x = 1 :: Int

-- 5)
--
-- `:sprint x` prints `_`, since `f` is a function and because type signature is
-- not concrete.
--
-- (CORRECT)
let f = \x -> x
let x = f 1

-- 6)
--
-- `:sprint x` prints `_`, since `f` is a function
--
-- (CORRECT)
let f :: Int -> Int; f = \x -> x
let x = f 1


-- Will printing this expression result in bottom?

-- 1)
--
-- No
--
-- (CORRECT)
snd (undefined, 1)

-- 2)
--
-- Yes, using `seq`
--
-- (CORRECT)
let x = undefined
let y = seq x 1 in snd (x, y)

-- 3)
--
-- Yes, since undefined is part of the spine
--
-- (CORRECT)
length $ [1..5] ++ undefined

-- 4)
--
-- No, since bottom is not part of the spine
--
-- (CORRECT)
length $ [1..5] ++ [undefined]

-- 5)
--
-- No, since `const` doesn't care about argument
--
-- (CORRECT)
const 1 undefined

-- 6)
--
-- Yes, because `seq`
--
-- (INCORRECT, due to outside-in evaluation)
const 1 (seq undefined 1)

-- 7)
--
-- Yes, because bottom is raised no matter input argument
--
-- (CORRECT)
const undefined 1


-- Make the expression bottom
x = undefined
y = "blah"
main = do
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    print $ snd (x, seq x y)
