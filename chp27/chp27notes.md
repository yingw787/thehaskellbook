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
