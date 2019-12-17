-- Phone.hs
--
-- (ALL FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
module Phone where

-- 1:
-- 2: ABC
-- 3: DEF
-- 4: GHI
-- 5: JKL
-- 6: MNO
-- 7: PQRS
-- 8: TUV
-- 9: WXYZ
-- 0: +_
-- *: ^
-- #: .,

-- 2 -> 'A'
-- 22 -> 'B'
-- 222 -> 'C'
-- 2222 -> '2'
-- 22222 -> 'A'

-- 1)
data Phone = Phone {buttons :: [Button]} deriving (Eq, Show)

-- Selecting shift.
data Mode = Shift | None deriving (Eq, Show)

-- Type synonyms for Char and Int.
type Key = Char
type Press = Int
-- Named accessors in data constructor
data Button = Button {key :: Key, output :: String} deriving (Eq, Show)

daPhone :: Phone
daPhone = Phone
    [
        Button '1' "1",
        Button '2' "ABC2",
        Button '3' "DEF3",
        Button '4' "GHI4",
        Button '5' "JKL5",
        Button '6' "MNO6",
        Button '7' "PQRS7",
        Button '8' "TUV8",
        Button '9' "WXYZ9s",
        Button '*' "^*",
        Button '0' " 0",
        Button '#' ".,#"
    ]

-- 2)
convo :: [String]
convo = [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]


keyMap :: Phone -> (Key, Press) -> Mode -> Char
keyMap = undefined

textOut :: Phone  -> [(Key, Press)] -> String
textOut = undefined

invButton :: Button -> [(Char, (Key, Press))]
invButton = undefined

invKeyMap :: Phone -> Char -> Maybe (Key, Press)
invKeyMap = undefined

keyPressIn :: Phone -> String -> [Maybe (Key, Press)]
keyPressIn = undefined

-- 3)
fingerTaps :: [Maybe (Key, Press)] -> Press
fingerTaps = undefined

-- 4)
mostPopularLetter :: String -> Char
mostPopularLetter = undefined

costOfMostPopularLetter :: Phone -> String -> Press
costOfMostPopularLetter = undefined

-- 5)
coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
