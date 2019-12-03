-- Sing.hs

-- Original problems
-- Module name is not capitalized
-- method `sndString` has incorrect return value type, should be `[Char]` and
-- not `Char`.
-- Comparison should be between lengths, and not direct string comparison.
-- `where` clause should invoke both x and y.

module sing where

fstString :: [Char] ++ [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> Char
sndString x = x ++ " over the rainbow"

sing if (x > y) then fstString x or sndString y
    where x = "Singin"
          x = "Somewhere"
