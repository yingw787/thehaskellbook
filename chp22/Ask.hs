-- Ask.hs
module Ask where

import Control.Applicative
import Control.Monad


newtype Reader r a = Reader { getReader :: r -> a }

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
ask :: Reader a a
ask = Reader id
