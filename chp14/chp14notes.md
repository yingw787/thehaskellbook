# Chapter 14

- Testing
    - `Hspec` and `QuickCheck`
    - Unit testing: Smallest atomic units of software independently from each
      other
    - Spec testing: tests when given declared input, result of the operation
      will be equal to the desired result. `Hspec`
    - Property testing: Tests formal properties of programs without requiring
      formal proofs by allowing you to express a truth-valued, universally
      quantified function which can be checked against randomly generated
      inputs. `QuickCheck`
        - If it passes, you can't be positive it will never fail because data is
          randomly generated (monte carlo assurance).
        - Not appropriate for all programs, e.g. no assertable, truth-valued
          properties of software.

********** BEGIN EXERCISE: RECURSIVE SUM **********

See `addition/RecursiveMul.hs`.

********** END EXERCISE: RECURSIVE SUM **********

- Enter `QuickCheck`

- See `addition/QuickCheck.hs`.

- Arbitrary instances

```haskell
-- QuickCheck relies on a typeclass called `Arbitrary` and a `newtype`
-- called `Gen`
*QuickCheck> :t arbitrary
arbitrary :: Arbitrary a => Gen a
-- We can use `sample` and `sample'` to see some random data
--
-- `IO` is necessary because it's using a global resource of random values
-- to generate the data, in order to generate different values each time
-- it's run, which is something pure functions cannot do.
*QuickCheck> :t sample
sample :: Show a => Gen a -> IO ()
*QuickCheck> :t sample'
sample' :: Gen a -> IO [a]
-- Typeclass `Arbitrary` creates a generator for `sample`.
*QuickCheck> sample (arbitrary :: Gen Int)
0
-2
2
-6
1
2
-12
-5
9
-16
-5
*QuickCheck> sample (arbitrary :: Gen Double)
0.0
-1.6742872872211467
-1.8738063855634695
-2.630861018607015
3.7680980592314857
-9.907710914447225
-11.849424351977355
0.14327002539994477
-10.419930089368275
7.299556213426548
18.182344695837205
-- Not specifying a default type results in a list of empty tuples.
-- This specifically works for GHCi, and not necessarily GHC.
*QuickCheck> sample arbitrary

<interactive>:8:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘()’
        (Show a0) arising from a use of ‘sample’ at <interactive>:8:1-16
        (Arbitrary a0)
          arising from a use of ‘arbitrary’ at <interactive>:8:8-16
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(sample arbitrary)’
      In a stmt of an interactive GHCi command:
        it <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
              (sample arbitrary)
()
()
()
()
()
()
()
()
()
()
()
*QuickCheck> trivialInt :: Gen Int; trivialInt = return 1
*QuickCheck> sample' trivialInt
[1,1,1,1,1,1,1,1,1,1,1]
-- Selects equal probability of 1, 2, and 3.
*QuickCheck> oneThroughThree :: Gen Int; oneThroughThree = elements [1, 2, 3]
*QuickCheck> sample' oneThroughThree
[2,3,2,2,2,2,2,2,1,3,2]
-- Selects higher probability of 2, with equal 1 and 3.
*QuickCheck> oneThroughThree' :: Gen Int; oneThroughThree' = elements [1, 2, 2, 2, 2, 3]
*QuickCheck> sample' oneThroughThree'
[2,2,2,2,2,3,2,2,2,2,2]
-- Selects higher probability of 1, with equal 2 and 3.
*QuickCheck> oneThroughThree'' :: Gen Int; oneThroughThree'' = elements [1, 1, 1, 1, 2, 3]
*QuickCheck> sample' oneThroughThree''
[2,2,3,1,1,3,1,3,1,1,3]
*QuickCheck> genBool :: Gen Bool; genBool = choose (False, True)
*QuickCheck> sample' genBool
[True,True,True,False,True,False,False,True,False,False,True]
*QuickCheck> genBool' :: Gen Bool; genBool' = elements [False, True]
*QuickCheck> sample' genBool'
[False,True,True,True,False,False,False,False,False,True,False]
*QuickCheck> genOrdering :: Gen Ordering; genOrdering = elements [LT, EQ, GT]
*QuickCheck> sample' genOrdering
[GT,LT,LT,EQ,EQ,GT,GT,LT,EQ,EQ,EQ]
*QuickCheck> genChar :: Gen Char; genChar = elements ['a'..'z']
*QuickCheck> sample' genChar
"pdkkbilrhwx"
*QuickCheck>
```

```haskell
*QuickCheck> :{
*QuickCheck| genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
*QuickCheck| genTuple = do
*QuickCheck|   a <- arbitrary
*QuickCheck|   b <- arbitrary
*QuickCheck|   return (a, b)
*QuickCheck| :}
*QuickCheck> :{
*QuickCheck| genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
*QuickCheck| genThreeple = do
*QuickCheck|   a <- arbitrary
*QuickCheck|   b <- arbitrary
*QuickCheck|   c <- arbitrary
*QuickCheck|   return (a, b, c)
*QuickCheck| :}
-- If you don't specify types for tuple samples in GHCi, it will default to
-- empty tuples. Otherwise, it will error out.
*QuickCheck> sample genTuple

<interactive>:62:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘()’
        (Show a0) arising from a use of ‘sample’ at <interactive>:62:1-15
        (Arbitrary a0)
          arising from a use of ‘genTuple’ at <interactive>:62:8-15
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(sample genTuple)’
      In a stmt of an interactive GHCi command:
        it <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
              (sample genTuple)

<interactive>:62:1: warning: [-Wtype-defaults]
    • Defaulting the following constraints to type ‘()’
        (Show b0) arising from a use of ‘sample’ at <interactive>:62:1-15
        (Arbitrary b0)
          arising from a use of ‘genTuple’ at <interactive>:62:8-15
    • In the first argument of ‘GHC.GHCi.ghciStepIO ::
                                  forall a. IO a -> IO a’, namely
        ‘(sample genTuple)’
      In a stmt of an interactive GHCi command:
        it <- GHC.GHCi.ghciStepIO :: forall a. IO a -> IO a
              (sample genTuple)
((),())
((),())
((),())
((),())
((),())
((),())
((),())
((),())
((),())
((),())
((),())
*QuickCheck> sample (genTuple :: Gen (Int, Float))
(0,0.0)
(-2,1.0756716)
(-3,2.3199508)
(-1,-0.16000462)
(-7,-3.166248)
(-6,6.1186585)
(-7,-3.5667067)
(11,5.2483764)
(-14,-8.892609)
(-7,-16.729694)
(7,-16.447632)
-- We can also ask for higher-kinded data structures as arbitrary types.
*QuickCheck> sample (genTuple :: Gen ([()], Char))
([],'\602169')
([],'\1027976')
([(),()],'\742288')
([(),(),()],'\346474')
([(),(),(),(),(),(),()],'7')
([(),(),(),(),(),()],'!')
([],'k')
([(),(),(),(),(),(),(),(),(),(),(),(),()],'\952940')
([(),(),(),(),(),(),(),(),(),(),(),(),(),()],'i')
([(),(),(),(),(),(),(),()],'\'')
([(),(),(),(),(),(),()],'T')
*QuickCheck>
```

- We can generate `Either` and `Maybe` types as well.

- Using `QuickCheck` without `Hspec`
