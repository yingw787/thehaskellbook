-- LearnParsers.hs
module LearnParsers where

-- 'https://hackage.haskell.org/package/trifecta ('stack install trifecta')
import Text.Trifecta


stop :: Parser a
stop = unexpected "stop"
