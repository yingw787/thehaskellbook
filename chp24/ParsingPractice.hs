-- ParsingPractice.hs
--
-- (Unless otherwise specified, answers are from answer key:
-- https://github.com/johnchandlerburnham/hpfp)
module ParsingPractice where

import Text.Trifecta
import Text.Parser.Combinators


stop :: Parser a
stop = unexpected "stop"

-- 1)
one = char '1' >> eof >> stop

oneTwo = char '1' >> char '2' >> eof >> stop

-- 2)
--
-- (PERSONAL NOTE: Not sure if the answer key makes sense to me...need to review
-- this problem a bit more)

-- 3)
string' :: String -> Parser String
string' "" = return ""
string' (x:xs) = do
    char x
    string' xs
    return (x:xs)
