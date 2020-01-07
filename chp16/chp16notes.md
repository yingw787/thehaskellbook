# Chapter 16

- Functors
    - Monoids taught us how to take an algebra and turn it into a typeclass.
    - Functors, Applicatives, and Monads will be similar.

    - Functor: Pattern of mapping over structure
    - `fmap`: Map, but over more data structures than just lists

- What's a functor?
    - Way to apply a function around some structure we don't want to alter
    - Apply to values within structure w/o affecting the structure itself
    - In Haskell, this is expressed usinga typeclass.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
--           1          2      3
-- 1: '(a -> b)' represents a function input argument
-- 2: 'f a' is a Functor f that takes type argument a.
-- 3: 'f b' is the return value. Same f as from 'f a', type argument 'b'
-- may but not necessarily refers to a different type (type can float from 'a').
```

- There's a whole lot of `fmap` goin' round

```haskell
-- 'fmap' has superset of operations of 'map' w.r.t. lists.
Prelude> map (\x -> x > 3) [1..6]
[False,False,False,True,True,True]
Prelude> fmap (\x -> x > 3) [1..6]
[False,False,False,True,True,True]
-- 'map' doesn't work around the 'Just' type, while 'fmap' does.
Prelude> map (+1) (Just 1)

<interactive>:3:11: error:
    • Couldn't match expected type ‘[b]’
                  with actual type ‘Maybe Integer’
    • In the second argument of ‘map’, namely ‘(Just 1)’
      In the expression: map (+ 1) (Just 1)
      In an equation for ‘it’: it = map (+ 1) (Just 1)
    • Relevant bindings include it :: [b] (bound at <interactive>:3:1)
Prelude> fmap (+1) (Just 1)
Just 2
-- 'fmap' works with tuples
--
-- (PERSONAL NOTE: This apparently works only with 2-tuples; changing the number
-- of values of the tuple appears to break in GHCi. I don't think this method is
-- mapping the function over all values of the tuple.)
Prelude> fmap (10/) (4, 5)
(4,2.0)
Prelude> rca = Right "Chris Allen"
Prelude> fmap (++ ", Esq.") rca
Right "Chris Allen, Esq."
-- You can see 'fmap' behavior generalized to different typeclasses by setting
-- config variable '-XTypeApplications'.
Prelude> :set -XTypeApplications
Prelude> :type fmap @Maybe
fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b
Prelude> :type fmap @(Either _)
fmap @(Either _) :: (a -> b) -> Either w a -> Either w b
Prelude>
```

- Let's talk about `f` baby
    - `f` must have the kind `* -> *` (is higher-kinded).
        - Each argument in the type signature must be a fully applied type.
        - `a` and `b` must have kind `*`.

- Shining star come into view

```haskell
Prelude> :k (->)
(->) :: TYPE q -> TYPE r -> *
Prelude>
```

```haskell
-- 'a' has kind * (standalone)
class Sumthin a where
    s :: a -> a

-- 'b' has kind * (standalone)
-- 'f' has kind * -> * (one argument to be fully applied)
-- 'g' has kind * -> * -> * -> * (three arguments to be fully applied)
class Else where
    e :: b -> f (g a b c)

-- 'e' has kind * -> * -> *
-- 'a' has kind *
-- 'c' has kind *
class Biffy where
    slayer :: e a b
        -> (a -> c)
        -> (b -> d)
        -> e c d
```

```haskell
-- 'Impish' is an illegally defined class, fails kindness check.
Prelude> :{
Prelude| class Impish v where
Prelude|   impossibleKind :: v -> v a
Prelude| :}

<interactive>:26:26: error:
    • Expected kind ‘k0 -> *’, but ‘v’ has kind ‘*’
    • In the type signature: impossibleKind :: v -> v a
      In the class declaration for ‘Impish’
-- 'AlsoImp' is an illegally defined class, fails kindness check.
Prelude> :{
Prelude| class AlsoImp v where
Prelude|   nope :: v a -> v
Prelude| :}

<interactive>:30:18: error:
    • Expecting one more argument to ‘v’
      Expected a type, but ‘v’ has kind ‘k0 -> *’
    • In the type signature: nope :: v a -> v
      In the class declaration for ‘AlsoImp’
Prelude>
```

********** BEGIN EXERCISES: BE KIND **********

1. 'a' has kind `*`.

2. 'b' has kind `* -> *`; 'T' has kind `* -> *`.

3. 'c' has kind `* -> * -> *`.

********** END EXERCISES: BE KIND **********

- A shining star for you to see

See `functors1.hs`.

- A shining star for you to see what your `f` can truly be

See `functors2.hs`.

- Functor Laws
    - Identity; `fmap id == id`
        - This checks whether if the identity method is passed to inner values
          of the structure, that the structure does not change.
        - Substitute `fmap` for your functor.
    - Composition; `fmap (f . g) == fmap f . fmap g`
        - Composing functions serially vs. breaking up composition with functor
          results in the same value.
        - Substitute `fmap` for your functor.

```haskell
-- 1)
fmap id "Hi Julie" == id "Hi Julie"

-- 2)
fmap ((+1) . (*2)) [1..5] -- [3, 5, 7, 9, 11]
fmap (+1) . fmap (*2) $ [1..5] -- [3, 5, 7, 9, 11]
```

- The Good, the Bad, and the Ugly

```haskell
Prelude> :{
Prelude| data WhoCares a =
Prelude|   ItDoesnt
Prelude|   | Matter a
Prelude|   | WhatThisIsCalled
Prelude|   deriving (Eq, Show)
Prelude| :}
Prelude>
Prelude> :{
-- This is a law-abiding functor.
Prelude| instance Functor WhoCares where
-- `fmap id ItDoesnt == id ItDoesnt`
Prelude|   fmap _ ItDoesnt = ItDoesnt
-- `fmap id WhatThisIsCalled == id WhatThisIsCalled`
Prelude|   fmap _ WhatThisIsCalled = WhatThisIsCalled
-- `fmap id (Matter _) == id (Matter _)`
Prelude|   fmap f (Matter a) = Matter (f a)
Prelude| :}
Prelude>
```

```haskell
-- This is an example of a law-breaking functor.
--
-- (PERSONAL NOTE: Why are law-breaking functors compilable?)
Prelude> :{
Prelude| instance Functor WhoCares where
Prelude|   fmap _ ItDoesnt = WhatThisIsCalled
Prelude|   fmap f WhatThisIsCalled = ItDoesnt
Prelude|   fmap f (Matter a) = Matter (f a)
Prelude| :}
-- Doesn't abide by the identity law, as can be seen below.
Prelude> fmap id ItDoesnt
WhatThisIsCalled
Prelude> fmap id WhatThisIsCalled
ItDoesnt
Prelude> fmap id ItDoesnt == id ItDoesnt
False
Prelude> fmap id WhatThisIsCalled == id WhatThisIsCalled
False
Prelude>
```

- If you need a function that can change the value and the structure, write a
  function, don't write a Functor
  - `Functor` provides property-based oblivousness to additional structure
    around types.

- Composition should just work

```haskell
-- Example of when law of composability fails for a Functor
Prelude> :{
-- CountingBad has one type argument, while Heisenberg has two
Prelude| data CountingBad a =
Prelude|   Heisenberg Int a
Prelude|   deriving (Eq, Show)
Prelude| :}
Prelude> :{
Prelude| instance Functor CountingBad where
Prelude|   fmap f (Heisenberg n a) =
Prelude|     Heisenberg (n + 1) (f a)
Prelude| :}
Prelude> u = "Uncle"
Prelude> oneWhoKnocks = Heisenberg 0 u
-- Here, we can see `fmap (f . g)` does not equal `fmap f . fmap g`.
Prelude> f = (++ " Jesse")
Prelude> g = (++ " lol")
Prelude> fmap (f . g) oneWhoKnocks
Heisenberg 1 "Uncle lol Jesse"
Prelude> fmap f . fmap g $ oneWhoKnocks
Heisenberg 2 "Uncle lol Jesse"
Prelude>
```

```haskell
Prelude> :{
Prelude| data CountingGood a =
Prelude|   Heisenberg Int a
Prelude|   deriving (Eq, Show)
Prelude| :}
Prelude> :{
Prelude| instance Functor CountingGood where
Prelude|   fmap f (Heisenberg n a) =
-- Don't mess with the variable that isn't present in the overarching type
-- signature (in this case, 'n'). This functor satisfies identity and
-- composability.
Prelude|     Heisenberg (n) (f a)
Prelude| :}
Prelude>
```

- Commonly used functors

```haskell
Prelude> :t const
const :: a -> b -> a
Prelude> replaceWithP = const 'p'
Prelude> replaceWithP 10000
'p'
Prelude> replaceWithP "woohoo"
'p'
Prelude> replaceWithP (Just 10)
'p'
-- `fmap`
Prelude> fmap replaceWithP (Just 10)
Just 'p'
Prelude> fmap replaceWithP Nothing
Nothing
Prelude> fmap replaceWithP [1, 2, 3, 4, 5]
"ppppp"
Prelude> fmap replaceWithP "Ave"
"ppp"
Prelude> fmap (+1) []
[]
Prelude> fmap replaceWithP []
""
Prelude> fmap replaceWithP (10, 20)
(10,'p')
Prelude> fmap replaceWithP (10, "woo")
(10,'p')
Prelude> negate 10
-10
Prelude> tossEmOne = fmap (+1) negate
Prelude> tossEmOne 10
-9
Prelude> tossEmOne (-10)
11
Prelude> tossEmOne' = (+1) . negate
Prelude> tossEmOne' 10
-9
Prelude> tossEmOne' (-10)
11
Prelude>
```

- The functors are stacked and that's a fact

```haskell
-- ~ > "is roughly equal to"
Prelude> n = Nothing
Prelude> w = Just "woohoo"
Prelude> ave = Just "Ave"
-- lms ~ List (Maybe (String))
Prelude> lms = [ave, n, w]
Prelude> replaceWithP = const 'p'
-- Replaces the entire list, since it's accepted as one argument
Prelude> replaceWithP lms
'p'
-- Replaces each element within the entire list w/o regard for the element
-- itself.
Prelude> fmap replaceWithP lms
"ppp"
-- Since all elements of this list are functors, you can do *another* `fmap`
-- in order to replace the value within each list element with 'p'.
Prelude> (fmap . fmap) replaceWithP lms
[Just 'p',Nothing,Just 'p']
-- Since each list element within the list contains a string (a list type),
-- another `fmap` replaces each element within that inner list with 'p'.
Prelude> (fmap . fmap . fmap) replaceWithP lms
[Just "ppp",Nothing,Just "pppppp"]
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude> :t (fmap . fmap)
(fmap . fmap)
  :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
Prelude> :t (fmap . fmap . fmap)
(fmap . fmap . fmap)
  :: (Functor f1, Functor f2, Functor f3) =>
     (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
Prelude>
```

- One more round for the P-Funkshun

See `ReplaceExperiment.hs`.

********** BEGIN EXERCISES: HEAVY LIFTING **********

See `HeavyLifting.hs`.

********** END EXERCISES: HEAVY LIFTING **********

- Transforming the unapplied type argument
    - We saw in tuples that only the latter argument was transformed
    - Then we saw when we created `Heisenberg` and composed an existing
      composite type that the additional type argument was not transformed
    - What really happens to other type arguments?

```haskell
Prelude> :{
-- Matches to `(,)`, the product type
Prelude| data Two a b =
Prelude|   Two a b
Prelude|   deriving (Eq, Show)
Prelude| :}
Prelude> :{
-- Matches to `Either`, the sum type
Prelude| data Or a b =
Prelude|   First a | Second b
Prelude|   deriving (Eq, Show)
Prelude| :}
-- Types do not have the proper kind-ness for Functor
Prelude> :k (,)
(,) :: * -> * -> *
Prelude> :k Either
Either :: * -> * -> *
Prelude> :k Two
Two :: * -> * -> *
Prelude> :k Or
Or :: * -> * -> *
-- By partially applying a type argument, you can reduce the kind-ness, which
-- enables functor construction.
Prelude> :k Either Integer
Either Integer :: * -> *
Prelude> :k Either Integer String
Either Integer String :: *
Prelude> :{
-- Functor declaration w/ compiler error
Prelude| instance Functor Two where
Prelude|   fmap = undefined
Prelude| :}

<interactive>:135:18: error:
    • Expecting one more argument to ‘Two’
      Expected kind ‘* -> *’, but ‘Two’ has kind ‘* -> * -> *’
    • In the first argument of ‘Functor’, namely ‘Two’
      In the instance declaration for ‘Functor Two’
Prelude> :{
-- Functor declaration w/o compiler error
Prelude| instance Functor (Two a) where
Prelude|   fmap = undefined
Prelude| :}
Prelude> :{
-- Illegal functor declaration, alters type argument 'a'.
Prelude| instance Functor (Two a) where
Prelude|   fmap f (Two a b) = Two $ (f a) (f b)
Prelude| :}

<interactive>:144:22: error:
    • Couldn't match expected type ‘Two a b’
                  with actual type ‘b0 -> Two a0 b0’
    • In the expression: Two $ (f a) (f b)
      In an equation for ‘fmap’: fmap f (Two a b) = Two $ (f a) (f b)
      In the instance declaration for ‘Functor (Two a)’
    • Relevant bindings include
        a :: a (bound at <interactive>:144:15)
        f :: a1 -> b (bound at <interactive>:144:8)
        fmap :: (a1 -> b) -> Two a a1 -> Two a b
          (bound at <interactive>:144:3)
Prelude> :{
-- Proper functor declaration, does not alter 'a'.
Prelude| instance Functor (Two a) where
Prelude|   fmap f (Two a b) = Two a (f b)
Prelude| :}
Prelude> :{
-- For the 'Or' functor, there are different values / types, but same constraint
-- applies. Not altering 'a' because 'a' is held constant during functor
-- declaration. This would probably change if 'b' was held constant during
-- functor declaration.
Prelude| instance Functor (Or a) where
Prelude|   fmap _ (First a) = First a
Prelude|   fmap f (Second b) = Second (f b)
Prelude| :}
Prelude>
```

- QuickChecking Functor instances

```haskell
-- Functor laws
fmap id = id
fmap (p . q) = (fmap p) . (fmap q)

-- Laws converted into QuickCheck properties
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b)-> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)
```

```haskell
Prelude> :{
Prelude| -- Laws converted into QuickCheck properties
Prelude| functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
Prelude| functorIdentity f = fmap id f == f
Prelude|
Prelude| functorCompose :: (Eq (f c), Functor f) => (a -> b)-> (b -> c) -> f a -> Bool
Prelude| functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)
Prelude| :}
Prelude> :{
-- Law of identity with concrete types
Prelude| let f :: [Int] -> Bool
Prelude|     f x = functorIdentity x
Prelude| :}
-- Law of composability with concrete types
Prelude> c = functorCompose (+1) (*2)
Prelude> li x = c (x :: [Int])
Prelude> import Test.QuickCheck
Prelude Test.QuickCheck> quickCheck f
+++ OK, passed 100 tests.
Prelude Test.QuickCheck> quickCheck li
+++ OK, passed 100 tests.
Prelude Test.QuickCheck>
```

********** BEGIN EXERCISES: INSTANCES OF FUNC **********

See `FuncInstances.hs`.

********** END EXERCISES: INSTANCES OF FUNC **********

- Ignoring possibilities
    - `Maybe` and `Either` functors are useful for when you want to ignore left
      (error / failure) cases.
    - You can use `fmap` seamlessly with them because they are functors.

```haskell
-- Pattern matching on `Maybe`
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

-- Refactoring the above using `fmap`
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

-- Refactoring even more in order to reduce the functions even more
incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

-- Refactoring to remove reference to `Maybe` within type signature
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
```

```haskell
Prelude> :{
Prelude| -- Pattern matching on `Maybe`
Prelude| incIfJust :: Num a => Maybe a -> Maybe a
Prelude| incIfJust (Just n) = Just $ n + 1
Prelude| incIfJust Nothing = Nothing
Prelude|
Prelude| showIfJust :: Show a => Maybe a -> Maybe String
Prelude| showIfJust (Just s) = Just $ show s
Prelude| showIfJust Nothing = Nothing
Prelude|
Prelude| -- Refactoring the above using `fmap`
Prelude| incMaybe :: Num a => Maybe a -> Maybe a
Prelude| incMaybe m = fmap (+1) m
Prelude|
Prelude| showMaybe :: Show a => Maybe a -> Maybe String
Prelude| showMaybe s = fmap show s
Prelude|
Prelude| -- Refactoring even more in order to reduce the functions even more
Prelude| incMaybe' :: Num a => Maybe a -> Maybe a
Prelude| incMaybe' = fmap (+1)
Prelude|
Prelude| showMaybe' :: Show a => Maybe a -> Maybe String
Prelude| showMaybe' = fmap show
Prelude|
Prelude| -- Refactoring to remove reference to `Maybe` within type signature
Prelude| liftedInc :: (Functor f, Num b) => f b -> f b
Prelude| liftedInc = fmap (+1)
Prelude|
Prelude| liftedShow :: (Functor f, Show a) => f a -> f String
Prelude| liftedShow = fmap show
Prelude| :}
-- Original pattern matching
Prelude> incIfJust (Just 1)
Just 2
Prelude> incIfJust (Nothing)
Nothing
Prelude> showIfJust (Just 1)
Just "1"
Prelude> showIfJust (Nothing)
Nothing
-- Refactored
Prelude> incMaybe (Just 1)
Just 2
Prelude> incMaybe Nothing
Nothing
Prelude> showMaybe (Just 1)
Just "1"
Prelude> showMaybe (Nothing)
Nothing
-- Refactored again
Prelude> incMaybe' (Just 1)
Just 2
Prelude> incMaybe' (Nothing)
Nothing
Prelude> showMaybe' (Just 1)
Just "1"
Prelude> showMaybe' (Nothing)
Nothing
-- Refactored yet again
Prelude> liftedInc (Just 1)
Just 2
Prelude> liftedInc (Nothing)
Nothing
Prelude> liftedShow (Just 1)
Just "1"
Prelude> liftedShow (Nothing)
Nothing
Prelude>
```

********** BEGIN EXERCISE: POSSIBLY **********

See `Possibly.hs`.

********** END EXERCISE: POSSIBLY **********

- Either

```haskell
-- Pattern matching
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Right n) = Right $ n + 1
incIfRight (Left e) = Left e

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Right s) = Right $ show s
showIfRight (Left e) = Left e

-- Using `fmap`
incEither :: Num a => Either e a -> Either e a
incEither m = fmap (+1) m

showEither :: Show a => Either e a -> Either e String
showEither s = fmap show s

-- eta-contract
incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- Remove specificity for `Either`
--
-- Method identical to the one implemented for `Maybe`, without changing
-- correctness!!
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show
```

********** BEGIN EXERCISE: EITHER **********

See `Either.hs`.

********** END EXERCISE: EITHER **********

- A somewhat surprising functor
    - `Data.Functor.Constant`
        - (PERSONAL NOTE: Not sure how to acquire this functor, Hackage says it
          is part of module `Data.Functor`, but I can't reference it in GHCi)

```haskell
-- 'b' is a phantom type
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

-- Apply a type variable to reduce kind-ness of `Constant`.
--
-- First type argument is part of the structure Functor skips over.
--
-- Since it satisfies identity and composability, `Constant` is a proper Functor
instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v
```

- More structure, more functors
