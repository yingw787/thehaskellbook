-- Programmer.hs
module Programmer where

data OperatingSystem =
        GnuPlusLinux
     |  OpenBSDPlusNevermindJustBSDStill
     |  Mac
     |  Windows
     deriving (Eq, Show)

data ProgLang =
        Haskell
    |   Agda
    |   Idris
    |   PureScript
    deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
    , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [
      GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Solution
allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]
