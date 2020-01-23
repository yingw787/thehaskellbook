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
