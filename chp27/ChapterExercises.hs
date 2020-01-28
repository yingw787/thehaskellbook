-- ChapterExercises.hs
module ChapterExercises where


-- What will `:sprint` output?

-- 1)
let x = 1

-- 2)
let x = ['1']

-- 3)
let x = [1]

-- 4)
let x = 1 :: Int

-- 5)
let f = \x -> x
let x = f 1

-- 6)
let f :: Int -> Int
    f = \x -> x

let x = f 1


-- Will printing this expression result in bottom?

-- 1)
snd (undefined, 1)

-- 2)
let x = undefined
let y = seq x 1 in snd (x, y)

-- 3)
length $ [1..5] ++ undefined

-- 4)
length $ [1..5] ++ [undefined]

-- 5)
const 1 undefined

-- 6)
const 1 (seq undefined 1)

-- 7)
const undefined 1


-- Make the expression bottom
x = undefined
y = "blah"
main = do
    print (snd (x, y))
