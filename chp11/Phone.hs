-- Phone.hs
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
data DaPhone = DaPhone

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

-- validButtons = "1234567890*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

-- 3)
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

-- 4)
mostPopularLetter :: String -> Char
mostPopularLetter = undefined

-- 5)
coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined
