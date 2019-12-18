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
