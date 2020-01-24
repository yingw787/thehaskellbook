# Chapter 26

- Monad transformers
    - Get comfortable with stacking monad transformers

- `MaybeT`

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma
```
