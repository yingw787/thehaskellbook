-- OuterInner.hs
module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1
