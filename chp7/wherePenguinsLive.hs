-- wherePenguinsLive.hs
module WherePenguinsLive where

-- Sum type.
data WherePenguinsLive =
    Galapagos |
    Antarctica |
    Australia |
    SouthAfrica |
    SouthAmerica deriving (Eq, Show)

-- Product type.
data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

-- is it South Africa? If so, return True.
-- Match only SouthAfrica, and fail everything else using a catch-all pattern
-- match.
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

-- Can also use pattern matching to unpack a product type.
gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

-- Creates some penguins.
humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

-- Use pattern matching to unpack data of type `Penguin`, and use pattern
-- matching to match on internal `WherePenguinsLives` value to generate a
-- result.
galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

-- `(||)` is the OR method.
antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
    (galapagosPenguin p) ||
    (antarcticPenguin p)

main :: IO ()
main = do
    -- print location of penguin, unpack attribute `WherePenguinsLive` inside of
    -- `Penguin`.
    print $ gimmeWhereTheyLive humboldt
    print $ gimmeWhereTheyLive gentoo
    print $ gimmeWhereTheyLive macaroni
    print $ gimmeWhereTheyLive little
    print $ gimmeWhereTheyLive galapagos

    print $ galapagosPenguin galapagos
    print $ galapagosPenguin little

    print $ antarcticPenguin gentoo
    print $ antarcticPenguin little

    print $ antarcticOrGalapagos little
    print $ antarcticOrGalapagos galapagos
    print $ antarcticOrGalapagos gentoo
