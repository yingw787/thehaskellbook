# Chapter 27

- Nonstrictness

- Laziness
    - By chapter's end, should be able to reason about reduction process of
      Haskell expressions and evaluate strictness / non-strictness

    - Haskell has non-strict (*not lazy*) evaluation
        - Expressions are reduced only when necessary

    - A thunk (placeholder in AST) is created for each expression when
      evaluation process begins
        - If it doesn't need to be evaluated, it gets GC'ed
        - It it does, it can be shared amongst expressions

- Observational Bottom Theory
    - Strictness is defined by ability to evaluate expressions that have bottom
      in them
    - Bottom is a convenient way of observing evaluation

- Standards and obligations
    - A truly lazy language memoizes all results of all functions it evaluates,
      resulting in unacceptably large amounts of memory usage.

    - Non-strict: Same behavior w.r.t bottom
        - You can have an expressions which results in a value, even if bottom
          or infinite data lurks within
    - Lazy: takes specific approach to how program executes

```haskell
fst (1, undefined)
-- 1
snd (undefined, 2)
-- 2
```

- Outside in, inside out
    - Strict languages evaluate inside out
    - Non-strict languages evaluate outside in

```haskell
possiblyKaboom = \f -> f fst snd (0, undefined)

-- booleans as lambdas
true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

possiblyKaboom' b =
    case b of
        True -> fst tup
        False -> snd tup
    where tup = (0, undefined)
```

- Outside-in evaluation is how you can touch the structure without touching the
  structure's contents (e.g. measuring size of list of bottoms)

- What does the other way look like?
    - See `OutsideIn.hs`.

- Can we make Haskell strict?
    - See `OutsideIn.hs`.

    - Case expressions in general force evaluation, since they discriminate on
      cases

- `seq` and ye shall find

```haskell
seq :: a -> b -> b

-- `seq` isn't `flip const`, it evaluates to weak head normal form; it is elided
-- to avoid churning your type signatures

-- Defined as
seq bottom b = bottom
seq literallyAnythingNotBottom b = b
```

- Since Haskell evaluation is demand-driven, can't guarantee that something will
  ever be evaluated
    - `seq` evaluates expressions up to weak head normla form
    - Simon Marlow: Parallel and Concurrent Programming in Haskell

- Case matching also chains evaluation

- Core dump
    - See `CoreDump.hs`.

```haskell
Prelude> :set -ddump-simpl
Prelude> :l CoreDump.hs
[1 of 1] Compiling CoreDump         ( CoreDump.hs, interpreted )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 24, types: 9, coercions: 0, joins: 0/0}

-- RHS size: {terms: 9, types: 2, coercions: 0, joins: 0/0}
discriminatory :: Bool -> Int
[GblId, Arity=1, Caf=NoCafRefs]
discriminatory
  = \ (b_a1up :: Bool) ->
      break<2>(b_a1up)
      case b_a1up of {
        False -> break<0>() GHC.Types.I# 0#;
        True -> break<1>() GHC.Types.I# 1#
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule1_r1vr :: GHC.Prim.Addr#
[GblId, Caf=NoCafRefs]
$trModule1_r1vr = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule2_r1vJ :: GHC.Types.TrName
[GblId, Caf=NoCafRefs]
$trModule2_r1vJ = GHC.Types.TrNameS $trModule1_r1vr

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule3_r1vK :: GHC.Prim.Addr#
[GblId, Caf=NoCafRefs]
$trModule3_r1vK = "CoreDump"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule4_r1vL :: GHC.Types.TrName
[GblId, Caf=NoCafRefs]
$trModule4_r1vL = GHC.Types.TrNameS $trModule3_r1vK

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
CoreDump.$trModule :: GHC.Types.Module
[GblId, Caf=NoCafRefs]
CoreDump.$trModule
  = GHC.Types.Module $trModule2_r1vJ $trModule4_r1vL



Ok, one module loaded.
```

```haskell
-- Same thing as above, but tidier
Prelude> :set -ddump-simpl
Prelude> :set -dsuppress-all
Prelude> :l CoreDump.hs
[1 of 1] Compiling CoreDump         ( CoreDump.hs, interpreted )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 24, types: 9, coercions: 0, joins: 0/0}

-- RHS size: {terms: 9, types: 2, coercions: 0, joins: 0/0}
discriminatory
discriminatory
  = \ b_a1up ->
      case b_a1up of {
        False -> I# 0#;
        True -> I# 1#
      }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule1_r1vr
$trModule1_r1vr = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule2_r1vJ
$trModule2_r1vJ = TrNameS $trModule1_r1vr

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule3_r1vK
$trModule3_r1vK = "CoreDump"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule4_r1vL
$trModule4_r1vL = TrNameS $trModule3_r1vK

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule
$trModule = Module $trModule2_r1vJ $trModule4_r1vL



Ok, one module loaded.
```

- Use `:set -ddump-simpl` and `:set -dsuppress-all` in order to view GHC Core
  and see what the GHC Core language representation of a Haskell compiled file
  resolves to in order to better understand performance.

- A little stricter now
    - See `OutsideIn.hs`.

********** BEGIN EXERCISES: EVALUATE **********

See `Evaluate.hs`.

********** END EXERCISES: EVALUATE **********

- Call by name, call by need
    - Call by value: argument expressions evaluated before entering a function
    - Call by name: argument expressions not evaluated before entering a
      function
    - Call by need: Expressions are evaluated once and shared

- Non-strict evaluation changes what we can do

```haskell
-- Works for strict and non-strict languages
Prelude> myList = [1, 2, 3]
Prelude> tail myList
[2,3]
-- Would crash on a strict language
Prelude> myList' = [undefined, 2, 3]
Prelude> tail myList'
[2,3]
```

- Thunk life
    - Thunk: reference to suspended computations
        - https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/heap-objects

- Not all values get thunked
    - GHCi `:sprint` command shows what's been evaluated already

```haskell
-- In the book's version of GHCi, myList is strictly evaluated and
-- `:sprint myList` is `[1, 2]`. My version of GHCi lazily evaluates myList
-- even though it is cast as a polymorphic type.
Prelude> myList = [1, 2] :: [Integer]
Prelude> :sprint myList
myList = _
Prelude> myList2 = [1, 2, 3]
Prelude> :sprint myList2
myList2 = _
Prelude> xs = [1, 2, id 1] :: [Integer]
Prelude> xs' = xs ++ undefined
-- xs' isn't evaluated because the function (++) is the outermost operator, and
-- not a data constructor. Therefore, it is not in WHNF.
Prelude> :sprint xs'
xs' = _
Prelude>
```

- Sharing is caring
    - Sharing reduces memory usage
    - GHC turns sharing on and off based on necessity and what it thinks will
      produce faster code
    - Compiler knows when code does or doesn't perform I/O

- Using trace to observe sharing
    - `Debug.Trace`: a way to put a `putStrLn` without having to reference `IO`

```haskell
Prelude> import Debug.Trace
Prelude Debug.Trace> a = trace "a" 1
Prelude Debug.Trace> b = trace "b" 2
-- `Debug.Trace` allows you to see how arguments are forced at evaluation.
-- For operations like addition, forcing order amongst pairs is not guaranteed.
Prelude Debug.Trace> a + b
b
a
3
```

- See `Trace.hs`.

- What promotes sharing
    - Names: name variables in order to share results
    - Kind-ness

- What subverts sharing
    - Inlining expressions using `:{\n:}`

- Why polymorphic values never seem to get forced
    - When typeclass constraints get simplified to GHC Core, they're just
      function arguments

- Prevent sharing on purpose
    - To prevent computations that have large memory footprints from persisting,
      especially when they reduce to a smaller value or when they're not needed
      anymore

- Forced sharing
    - Give expression a name (e.g. using `let`)

- Refutable and irrefutable patterns
    - Irrefutable pattern: one that will never fail to match
    - Refutable pattern: one that might fail to match
    - Pattern != function

- Lazy patterns
