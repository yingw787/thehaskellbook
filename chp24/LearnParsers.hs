-- LearnParsers.hs
module LearnParsers where

-- 'https://hackage.haskell.org/package/trifecta ('stack install trifecta')
import Text.Trifecta


stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop
--
-- use the sequencing operator in order to combine two parsers
--
-- (>>) :: Monad m => m a -> m b -> m b
--
-- The first return value is thrown away, but the monadic context from the first
-- argument remains.

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "stop:"
    testParse stop

    pNL "one:"
    testParse one

    pNL "one':"
    testParse one'

    pNL "oneTwo:"
    testParse oneTwo

    pNL "oneTwo':"
    testParse oneTwo'
