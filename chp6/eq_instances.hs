-- eq_instances.hs

module EqInstances where

-- (1)
data TisAnInteger = TisAn Integer
-- (2)
data TwoIntegers = Two Integer Integer
-- (3)
data StringOrInt = TisAnInt Int | TisAString String
-- (4)
data Pair a = Pair a a
-- (5)
data Tuple a b = Tuple a b
-- (6)
data Which a = ThisOne a | ThatOne a
-- (7)
data EitherOr a b = Hello a | Goodbye b


-- (1)
-- instance Eq TisAnInteger where
--     (==) (TisAn a) (TisAn a') = True
--
-- (INCORRECT) (PERSONAL NOTE: Not sure why I thought immediate comparison
-- between two different elements should default to True...)
--
-- Solution from https://github.com/johnchandlerburnham/hpfp:
--
instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = (==) a a'

-- (2)
-- instance Eq TwoIntegers where
--     (==) (Two a) (Two a') = (==) a a'
--
-- (INCORRECT)
--
-- Solution from https://github.com/johnchandlerburnham/hpfp:
--
instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = (==) (x, y) (x', y')

-- (3)
instance Eq StringOrInt where
    (==) (TisAnInt someInt) (TisAnInt someInt') = (==) someInt someInt'
    (==) (TisAString someStr) (TisAString someStr') = (==) someStr someStr'
-- (PARTIALLY CORRECT, forgot catch-all case, added from solution below:
-- https://github.com/johnchandlerburnham/hpfp)
    (==) _ _ = False

-- (4)
instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = (==) (x, y) (x', y')
-- (PARTIALLY CORRECT, do not need to apply constrained polymorphism apparently.
-- Answer from https://github.com/johnchandlerburnham/hpfp)
--
-- instance Eq Pair where
--     Pair a b == Pair x y = (a, b) == (x, y)
--
-- Answer key incorrect, different answer key does apply constrained
-- polymorphism.

-- (5)
-- instance Eq Tuple where
--     (==) (Tuple a b) (Tuple a' b') = (==) (a, b) (a', b')
--
-- (INCORRECT, compile-time error. Try again.)
--
-- instance Eq (a, b) => Eq (Tuple a b) where
--     (==) (Tuple a b) (Tuple a' b') = (==) (a, b) (a', b')
--
-- (INCORRECT, compile-time error. Try again.)
--
-- instance Eq a => Eq b => Eq (Tuple a b) where
--     (==) (Tuple a b) (Tuple a' b') = (==) (a, b) (a', b')
--
-- (INCORRECT, answer from https://github.com/johnchandlerburnham/hpfp below.)
--
-- instance Eq Tuple where
--     (==) (Tuple a b) (Tuple x y) = (==) (a, b) (x, y)
--
-- (ANSWER KEY INCORRECT, TRY DIFFERENT ANSWER KEY:
-- https://github.com/CarlosMChica/HaskellBook)
--
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = (==) (a, b) (a', b')

-- (6)
instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = (==) a a'
    (==) (ThatOne b) (ThatOne b') = (==) b b'
    (==) _ _ = False
-- (CORRECT)

-- (7)
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = (==) a a'
    (==) (Goodbye b) (Goodbye b') = (==) b b'
    (==) _ _ = False
-- (CORRECT)
