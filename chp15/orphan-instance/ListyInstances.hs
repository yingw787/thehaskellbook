-- orphan-instance/ListyInstances.hs
module ListyInstances where

import Data.Monoid
import Listy

instance Semigroup (Listy a) where
    (<>) (Listy l) (Listy l') = Listy $ mappend l l'

instance Monoid (Listy a) where
    mempty = Listy []
