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

main :: IO ()
main = do
    -- print location of penguin, unpack attribute `WherePenguinsLive` inside of
    -- `Penguin`.
    print $ gimmeWhereTheyLive humboldt
    print $ gimmeWhereTheyLive gentoo
    print $ gimmeWhereTheyLive macaroni
    print $ gimmeWhereTheyLive little
    print $ gimmeWhereTheyLive galapagos
