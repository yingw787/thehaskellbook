-- Sing.hs

-- Original problems
-- Module name is not capitalized
-- method `sndString` has incorrect return value type, should be `[Char]` and
-- not `Char`.
-- Comparison should be between lengths, and not direct string comparison.
-- `where` clause should invoke both x and y.

-- Additional problems
-- Ternaries should have else clauses, not an or clause.
-- method `fstString` should not use `++` in type signature, but `->` instead.
-- Comparison should not necessarily be between lengths, as strings can compare
-- alphabetically.

module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
    where x = "Singin"
          y = "Somewhere"
