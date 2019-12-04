-- datatypes.hs

module DataTypes where

data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- (1)
-- DOES NOT TYPECHECK
-- phew = Papu "chases" True
--
-- Need to instantiate as proper types beforehand.
phew = Papu (Rocks "chases") (Yeah True)

-- (2)
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- (3)
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = (==) p p'

-- (4)
-- DOES NOT TYPECHECK
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = (>) p p'
--
-- Need to derive Papu, Rocks, Yeah from Ord instead of Eq, and implement
-- instance Ord. Or something...I'm not quite sure.
