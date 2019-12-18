# Chapter 13

- Building projects

- Modules
    - Contain datatypes, type synonyms, typeclasses, typeclass instances, and
      values.

- Making packages with `stack`
    - `cabal`: Common Architecture for Building Applications and Libraries,
      package manager
    - `stack`: program for building Haskell projects.

- Stackage uses LTS snapshots of Haskell packages, whereas Hackage does not.

- Working with a basic project
    - `stack build`: Build a stack project
    - `stack setup`: Run before build step if conflicting dependencies arise

- (PERSONAL NOTE: `stack ghci` only detects Main if you're already in the
  `hello/` package. So does `stack exec`.)

- Executable stanzas in Cabal files

```cabal
executable hello
--         1
  hs-source-dirs:      src
-- 2
  main-is:             Main.hs
-- 3
  default-language:    Haskell2010
-- 4
  build-depends:       base >= 4.7 && < 5
-- 5

-- 1: Name of the executable
-- 2: Source code directory
-- 3: Look for a `main` function inside this file
-- 4: Haskell version
-- 5: dependencies
```

- Libraries can be reused by the compiler, while executables are directly run by
  the operating system.

- Making our project a library
    - (PERSONAL NOTE: I didn't get the same error authors did about GHC
      suggestion)

- More on importing modules
    - Imported modules are top-level declarations, and have scope throughout the
      module
    - Effect of multiple import declarations is cumulative
    - Ordering of import declarations is irrelevant

- If you run GHCi using bash command `stack ghci --ghci-options
  -XNoImplicitPrelude`, then you can use bare interpreter shell without having
  Prelude library

- To import one method in one package, use syntax `import Data.Bool (bool)`
  (`not` is not in scope here); use `:browse` in GHCi in order to see what is
  available in each module

- Qualified Imports
    - `import qualified Data.Bool` to use `bool` as `Data.Bool.bool` and
      `Data.Bool.not`.
    - `import qualified Data.Bool as B` to use `bool` as `B.bool` and `B.not`.

********** BEGIN EXERCISES: CHECK YOUR UNDERSTANDING **********

(CORRECT-ISH BY ANSWER KEY)

1. `forever`, `when`

2. `Database.Blacktip.Types`, `Data.Bits`

3. Database types

4. Below:

a) `MV`, `FPC`, `CC` (INCORRECT, SHOULD LIST FULL NAME OF MODULE)
b) `Filesystem`
c) `Control.Monad`

********** END EXERCISES: CHECK YOUR UNDERSTANDING **********

- `do` syntax and IO
    - Enables us to Sequence monadic actions.

```haskell
concatUserInput = do
    x1 <- getLine
    x2 <- getLine
    return (x1 ++ x2)
```

- `do` : block of IO actions.
- `x1` and `x2` are bound with `<-` to the first and second `getLine` method
  calls.
- `return` concludes the `do` block with a value, and places it in `IO()`.

```haskell
twoo :: IO Bool
twoo = do
    c <- getChar
    c' <- getChar
    -- THIS LINE IS ILLEGAL
    -- c == c'
    return (c == c')

-- If you don't want to return anything from the `do` block
twoo' :: IO ()
twoo' = do
    c <- getChar
    c' <- getChar
    if (c == c')
        then putStrLn "True"
        else return ()
```

- `do` blocks look like imperative programming, but explicitly requires having
  IO () and generating side effects.

- Don't overuse `do` blocks.
