-- identity.hs
module Identity where

data Identity a = Identity a

-- The code below has a compile-time error, described when loading into GHCi.
--
-- identity.hs:7:39: error:
--     • No instance for (Eq a) arising from a use of ‘==’
--       Possible fix: add (Eq a) to the context of the instance declaration
--     • In the expression: v == v'
--       In an equation for ‘==’: (==) (Identity v) (Identity v') = v == v'
--       In the instance declaration for ‘Eq (Identity a)’
--   |
-- 7 |     (==) (Identity v) (Identity v') = v == v'
--   |                                       ^^^^^^^
--
-- instance Eq (Identity a) where
--     (==) (Identity v) (Identity v') = v == v'

-- v and v' are both of type a, but we don't know anything about a, including
-- whether it has Eq typeclass. So, apply same typeclass constraint syntax as
-- with functions:

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'
