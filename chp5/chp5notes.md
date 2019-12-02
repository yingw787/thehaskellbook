# Chapter 5

- Types

- You cannot create untyped data in Haskell

- Typed lambda calculus (System F) in the 1970s
    - Haskell improves upon System F
        - Allows general recursion
        - Hindley-Milner type system

- Type systems in logic / mathematics designed to impose constraints that
  enforce correctness.
- Well-designed type systems help eliminate some classes of errors
- Also restructure concerns such as what a conditional over a non-Boolean value
  might be
- Type system defines associations between different parts of a program
- Checks all parts fit together in logically consistent, provably correct way.

- Haskell applies static typing, which means type-checking occurs at compile
  time.
- Doesn't prevent possibilities for runtime errors and exceptions.
- Type system reduces number and kinds of tests you must write.

- Good type systems also enable compiler optimizations, because the compiler can
  know and predict certain things about program execution.
- Types can also act as documentation and improve hallway usability / pair
  programming.

- Type systems will require a lot of upfront work, with a later payoff: safer,
  more maintainable code.

```haskell
Prelude> :type 't'
't' :: Char
Prelude> :type "julie"
"julie" :: [Char]
Prelude> :type True
True :: Bool
-- 13 may look like an integer, but that would only allow us to use it in
-- computations that take integers. Compiler gives it the type with the broadest
-- applicability (most polymorphic) and constrains it based on that
-- (constrained polymorphism).
Prelude> :type 13
13 :: Num p => p
-- Types can also match function type signatures.
Prelude> :type not
not :: Bool -> Bool
Prelude>
```

```haskell
Prelude> :info (->)
-- (->) is the type constructor for Haskell methods.
-- Has no data constructors and has no data constructors.
-- There are no data constructors because the value that shows up at term level
-- is the function. Functions are values.
--
-- a is the input to the function, b is the return value of the function.
data (->) (a :: TYPE q) (b :: TYPE r) 	-- Defined in ‘GHC.Prim’
-- Note similarity between (->) and (,).
Prelude> :info (,)
data (,) a b = (,) a b 	-- Defined in ‘GHC.Tuple’
Prelude>
```

- Functions are different from regular values because they can be applied.
- `(->)` is an infix operator that has two parameters and associates to the right
    - Function *application* is still left-associative.

```haskell
Prelude> :type fst
-- Method `fst` to get the first element of a two-tuple
--
-- First parameter of `fst` is '(a, b)' (which is a two-tuple with its own
-- type definition), an infix operator that defines the actual function,
-- and the return value 'a', which is the same 'a' that is in the tuple.
--
-- We know that it is the same 'a' that is returned because we see the input 'a'
-- and output 'a' are the same type, and nothing happens between input and
-- output. Note that the second deduction is made from the type signature (?).
fst :: (a, b) -> a
-- Another method `length` takes in an input of subclass `Foldable`, and return
-- an integer. Method `length` does not care about internal arguments of 'a'.
Prelude> :type length
length :: Foldable t => t a -> Int
Prelude>
```

- Compiler does not limit types to concrete ones, but rather limits based on
  polymorphic type variable, to enable the variable to represent any concrete
  type.
- For numbers, this means we can represent the same numeric value in different
  types.

```haskell
Prelude> fifteen = 15
Prelude> :type fifteen
fifteen :: Num p => p
Prelude> fifteenInt = fifteen :: Int
Prelude> :type fifteenInt
fifteenInt :: Int
Prelude> fifteenDouble = fifteen :: Double
Prelude> :type fifteenDouble
fifteenDouble :: Double
Prelude>
```

- `Int` and `Double` both have an instance of the `Num` type class, which is why
  we can cast `Num a => a` 'fifteen' to both 'Int' and 'Double'.

- Constrained polymorphism also means we can apply `(+)` where we could not if we
  used concrete types only, to avoid type translation errors:

```haskell
-- 'fifteen' is cast to Double.
Prelude> fifteenDouble + fifteen
30.0
-- 'fifteen' is cast to Int.
Prelude> fifteenInt + fifteen
30
Prelude> fifteenDouble + fifteenDouble
30.0
Prelude> fifteenInt + fifteenInt
30
-- Type translation error by adding two different concrete types.
-- Since we applied 'fifteenDouble' as the first argument, 'Double' is the
-- expected type for the second argument, but the actual type was 'Int'.
Prelude> fifteenDouble + fifteenInt

<interactive>:21:17: error:
    • Couldn't match expected type ‘Double’ with actual type ‘Int’
    • In the second argument of ‘(+)’, namely ‘fifteenInt’
      In the expression: fifteenDouble + fifteenInt
      In an equation for ‘it’: it = fifteenDouble + fifteenInt
Prelude>
```

********** BEGIN EXERCISES: TYPE MATCHING **********

Match the function to its type signature, then check your work in GHCi.

1. Functions:

    a) `not`
    b) `length`
    c) `concat`
    d) `head`
    e) (<)

2. Type signatures:

    a) `_ :: [a] -> a`
    b) `_ :: [[a]] -> [a]`
    c) `_ :: Bool -> Bool`
    d) `_ :: [a] -> Int`
    e) `_ :: Ord a => a -> a -> Bool`

__________

2a) -> 1d)
2b) -> 1c)
2c) -> 1a)
2d) -> 1b)
2e) -> 1e)

```haskell
-- CORRECT
Prelude> :type not
not :: Bool -> Bool
-- CORRECT
Prelude> :type length
length :: Foldable t => t a -> Int
-- CORRECT (?)
Prelude> :type concat
concat :: Foldable t => t [a] -> [a]
-- CORRECT (?)
Prelude> :type head
head :: [a] -> a
-- CORRECT
Prelude> :type (<)
(<) :: Ord a => a -> a -> Bool
Prelude>
```

********** END EXERCISES: TYPE MATCHING **********

- Lambda calculus does not have support for methods taking multiple arguments.
  Instead, functions with multiple arguments are syntactic sugar for curried
  methods.
- Currying is nesting of multiple functions to allow the illusion of
  multiple-parameter functions.

```haskell
-- `(+)` is a curried method, because it looks like it accepts two arguments,
-- when in reality it applies to one argument, which returns another function
-- that accepts the next argument, which actually creates the return value.
Prelude> :type (+)
(+) :: Num a => a -> a -> a
Prelude>
```

- The type constructor for functions makes currying *default* in Haskell, as it
  is an infix operator and right-associative.

```haskell
Prelude> :type (+)
(+) :: Num a => a -> a -> a
-- associates to: `(+) :: Num a => a -> (a -> a)`
-- Parentheses here group parameters into argument / result, as every arrow
-- must have one argument and one result.
Prelude>
```

- Partial application: apply only some of a function's arguments. This enables
  function reuse and composition.

```haskell
-- Define a method `addStuff` that takes two arguments and returns the sum of
-- both arguments and 5.
Prelude> :{
Prelude| addStuff :: Integer -> Integer -> Integer
Prelude| addStuff a b = a + b + 5
Prelude| :}
-- Takes one argument, returns a function that takes one argument, which
-- returns one result.
Prelude> :type addStuff
addStuff :: Integer -> Integer -> Integer
-- The above is equivalent to `addStuff :: Integer -> (Integer -> Integer)`.
-- Resolving the latter leads to addTen given input is 5.
Prelude> addTen = addStuff 5
Prelude> :type addTen
addTen :: Integer -> Integer
Prelude> fifteen = addTen 5
Prelude> :type fifteen
fifteen :: Integer
-- `fifteen` is equal to `addStuff 5 5`, because `addTen` is equal to
-- `addStuff 5`.
Prelude> fifteen
15
Prelude> addTen 5
15
Prelude> addStuff 5 5
15
Prelude>
```

- Examining partial application with a function that is not commutative:

```haskell
Prelude> :{
Prelude| subtractStuff :: Integer -> Integer -> Integer
Prelude| subtractStuff x y = x - y - 10
Prelude| :}
Prelude> subtractOne = subtractStuff 1
Prelude> :type subtractOne
subtractOne :: Integer -> Integer
-- result is equivalent to `subtractStuff 1 11`.
Prelude> result = subtractOne 11
Prelude> result
-20
Prelude>
```

- Un-currying is possible by replacing two functions with a two-tuple.
- Method signature would change from `Num a => a -> a -> a` to `Num => (a, a) ->
  a`. Representing single argument as the overarching tuple that carries
  multiple arguments.
    - PERSONAL QUESTION: Since a tuple can only have one type, how does
      uncurrying with multiple arguments of multiple types work out?

```haskell
Prelude> :{
Prelude| nonsense :: Bool -> Integer
Prelude| nonsense True = 805
Prelude| nonsense False = 31337
Prelude| :}
Prelude> :{
Prelude| curriedFunction :: Integer -> Bool -> Integer
Prelude| curriedFunction i b = i + (nonsense b)
Prelude| :}
Prelude> :{
Prelude| uncurriedFunction :: (Integer, Bool) -> Integer
Prelude| uncurriedFunction (i, b) = i + (nonsense b)
Prelude| :}
Prelude> :{
Prelude| anonymous :: Integer -> Bool -> Integer
Prelude| anonymous = \i b -> i + (nonsense b)
Prelude| :}
Prelude> :{
Prelude| anonNested :: Integer -> Bool -> Integer
-- `anonNested` does not leverage automatic currying, all manual currying.
Prelude| anonNested = \i -> \b -> i + (nonsense b)
Prelude| :}
-- `curriedFunction`, `anonymous`, and `anonNested` are all the same function
-- giving the same result.
Prelude> curriedFunction 10 False
31347
Prelude> uncurriedFunction (10, False)
31347
Prelude> anonymous 10 False
31347
Prelude> anonNested 10 False
31347
Prelude>
```

- You can curry methods using `curry`, and uncurry methods using `uncurry`:s

```haskell
-- Below is eqivalent to (curry f) a b = f (a, b)
Prelude> curry f a b = f (a, b)
Prelude> :t curry
curry :: ((a, b) -> t) -> a -> b -> t
Prelude> :t fst
fst :: (a, b) -> a
Prelude> :t curry fst
curry fst :: t -> b -> t
Prelude> fst (1, 2)
1
Prelude> curry fst 1 2
1
-- Below is equivalent to (uncurry f) (a, b) = f a b
Prelude> uncurry f (a, b) = f a b
Prelude> :t uncurry
uncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> (+) 1 2
3
Prelude> uncurry (+) (1, 2)
3
Prelude>
```

- Sectioning: partial application of infix operators, which has a special syntax
  and can result in different associativity behaviors during function
  application:

```haskell
Prelude> x = 5
Prelude> y = (2^)
Prelude> z = (^2)
-- (2^) 5 -> 2 ^ 5
Prelude> y x
32
-- (^2) 5 -> 5 ^ 2
Prelude> z x
25
-- Doesn't just work for numeric types. Associativity applies here too.
Prelude> celebrate = (++ " woot!")
Prelude> celebrate "naptime"
"naptime woot!"
Prelude> _celebrate = ("its " ++)
Prelude> _celebrate "naptime!"
"its naptime!"
Prelude>
-- You can apply sectioning to a prefix method by turning the prefix method
-- into an infix method using backticks.
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> elem 9 [1..10]
True
Prelude> 9 `elem` [1..10]
True
Prelude> c = (`elem` [1..10])
Prelude> c 9
True
Prelude> c 25
False
Prelude>
```

********** BEGIN EXERCISES: TYPE ARGUMENTS **********

Given a function and its type, tell us what type results from applying some or
all of the arguments. You can check your work in the REPL like this:

```haskell
Prelude> f :: a -> a -> a -> a; f = undefined
Prelude> x :: Char; x = undefined
Prelude> :type f x
```

*You can check the types of things that aren't implemented yet*, as long as you
give GHCi *undefined* to bind the signature to.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `x` is `Char`, then
   the type of `f x` is:

    a) `Char -> Char -> Char`
    b) `x -> x -> x -> x`
    c) `a -> a -> a`
    d) `a -> a -> a -> Char`

    __________

    a), because consistent `a` means that the same type must be applied
    throughout, and three infix function applications implies multiple
    arguments, and therefore passing one argument resolves one variable.

    (CORRECT)

    ```haskell
    Prelude> f :: a -> a -> a -> a; f = undefined
    Prelude> x :: Char; x = undefined
    Prelude> :type f x
    f x :: Char -> Char -> Char
    ```

2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"`
   is:

    a) `String`
    b) `Char -> String`
    c) `Int`
    d) `Char`

    __________

    d), because the method is fully applied with three arguments to get a beta
    normal result, and because `(->)` is right-associative, the second argument
    `'c'` will be `'b'` and that will be the type of the return value.

    (CORRECT)

    ```haskell
    Prelude> g :: a -> b -> c -> b; g = undefined
    -- x y z must be concrete types! Cannot cast as Num.
    Prelude> x :: Int; x = undefined
    Prelude> y :: Char; y = undefined
    Prelude> z :: String; z = undefined
    Prelude> :type g x y z
    g x y z :: Char
    Prelude>
    ```

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h
   1.0 2` is:

    a) `Double`
    b) `Integer`
    c) `Integral b => b`
    d) `Num b => b`

    Note that because the type variables `a` and `b` are different, the compiler
    *must* assume that the types could be different.

    __________

    a), because `h` can be parenthesized as `(Num a, Num b) => a -> (b -> b)`,
    and first argument applied to the method will define the return type.

    (INCORRECT) (PERSONAL NOTE: Maybe parentheses make a difference during
    method definition.)

    ```haskell
    Prelude> h :: (Num a, Num b) => a -> b -> b; h = undefined
    Prelude> x :: Double; x = undefined
    Prelude> y :: Int; y = undefined
    Prelude> :type h x y
    h x y :: Int
    ```

4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1
   (5.5 :: Double)` is:

    a) `Integer`
    b) `Fractional b => b`
    c) `Double`
    d) `Num b => b`

    __________

    c), because latter argument is explicitly cast to concrete type `Double`,
    and from question 3), the latter argument is applied as 'b'. Although
    because this doesn't have parentheses, the ordering of the application may
    be determined by the `(->)` infix operator.

    (CORRECT) (Parentheses are part of method definition, will not change based
    on arguments.)

    ```haskell
    Prelude> h :: (Num a, Num b) => a -> b -> b; h = undefined
    Prelude> p :: Int; p = undefined
    Prelude> q :: Double; q = undefined
    Prelude> :type h p q
    h p q :: Double
    ```

5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
   `jackal "keyboard" "has the word jackal in it" is:

    a) `[Char]`
    b) `Eq b => b`
    c) `b -> [Char]`
    d) `b`
    e) `Eq b => b -> [Char]`

    __________

    a), because the method `jackal` is fully applied with two arguments, and
    argument `'a'` defines the type of the return value.

    (CORRECT)

    ```haskell
    Prelude> jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
    Prelude> x :: [Char]; x = undefined
    Prelude> y :: [Char]; y = undefined
    Prelude> :type jackal x y
    jackal x y :: [Char]
    ```

6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
   `jackal "keyboard"` is:

    a) `b`
    b) `Eq b => b`
    c) `[Char]`
    d) `b -> [Char]`
    e) `Eq b => b -> [Char]`

    __________

    e), because after applying the first argument, a partially applied function
    is returned that still needs to apply a second argument of type `Eq`.

    (CORRECT)

    ```haskell
    Prelude> jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
    Prelude> x :: [Char]; x = undefined
    Prelude> y :: [Char]; y = undefined
    Prelude> :type jackal x
    jackal x :: Eq b => b -> [Char]
    ```

7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel 1 2` is:

    a) `Integer`
    b) `Int`
    c) `a`
    d) `(Num a, Ord a) => a`
    e) `Ord a => a`
    f) `Num a => a`

    __________

    e), because of constrained polymorphism, the type only needs to be the same
    as `'a'`, which is defined in type signature as only constrained as `Ord a`.

    (CORRECT)

    ```haskell
    Prelude> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
    Prelude> x :: Ord a => a; x = undefined
    Prelude> y :: Num b => b; y = undefined
    Prelude> :type kessel x y
    kessel x y :: Ord a => a
    ```

8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel 1 (2 :: Integer)` is:

    a) `(Num a, Ord a) => a`
    b) `Int`
    c) `a`
    d) `Num a => a`
    e) `Ord a => a`
    f) `Integer`

    __________

    e), because the return value is constrained by the first argument applied,
    and because there is no explicit type casting as part of the method
    signature.

    (CORRECT)

    ```haskell
    Prelude> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
    Prelude> x :: Ord a => a; x = undefined
    Prelude> z :: Integer; z = undefined
    Prelude> :type kessel x z
    kessel x z :: Ord a => a
    ```

9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel (1 :: Integer) 2` is:

    a) `Num a => a`
    b) `Ord a => a`
    c) `Integer`
    d) `(Num a, Ord a) => a`
    e) `a`

    __________

    c), because the input argument type is more constrained than either input
    argument in the method definition, and therefore the return value will have
    the constrained type applied.

    (CORRECT)

    ```haskell
    Prelude> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
    Prelude> x :: Ord a => a; x = undefined
    Prelude> z :: Integer; z = undefined
    Prelude> :type kessel z x
    kessel x z :: Integer
    ```

********** END EXERCISES: TYPE ARGUMENTS **********

- Polymorphic type variables enable expressions that can accept arguments and
  return results of different types w/o having to write variations on the same
  expression for different types.

- Difference between polymorphism and generics:
  https://stackoverflow.com/a/2423355 (also pasted below)

> Polymorphism is a property of classes, in that they implement a common
> interface, or are derived from a base class, implementing virtual methods in a
> different way to reflect the different behavior of derived classes.

> Generics is a property of an algorithm, or a class implementing an algorithm
> (sort) or a common operation (lists), requiring the classes they deal with to
> have certain methods, properties, or interfaces.

(Honestly, Haskell polymorphism sounds a lot like object-oriented generics, was
confused as to this point and went to #haskell on IRC)

```irc
13:34 < yingw787> I have a question about polymorphism vs. generics
13:35 < yingw787> the author discusses polymorphic type variables allowing expressions to be written once and applied to multiple types
13:35 < yingw787> this is akin to object-oriented generics right, and less so object-oriented polymorphism?
13:35 < yingw787> https://stackoverflow.com/a/2423355
13:35 < ChaiTRex> yingw787: No, this is like an interface in Java.
13:36 < ChaiTRex> yingw787: You have a set of functions that can apply to any of several types. The types each implement the set of
                  functions.
13:37 < yingw787> ChaiTRex: So what you're saying is that the method definition you write is resolved to concrete types by all types that
                  exist under those polymorphic constraints?
13:38 < yingw787> And that the method you're writing is not actually being run, it's a different method evaluated at compile-time?
13:38 < ChaiTRex> Well, it's resolved to concrete functions.
13:39 < yingw787> I am using GHCi to run everything at the moment, which is why my understanding of compile-time / run-time stuff may be
                  a bit wonky
13:39 < ChaiTRex> Nothing is evaluated at compiletime.
13:39 < dsal> Ghci muddies a few concepts, yeah
13:39 < ChaiTRex> You have a set of functions you want to be able to use with a set of types.
13:40 < ChaiTRex> That's a typeclass. It has functions you can apply to any type that implements the typeclass.
13:40 < ChaiTRex> The types themselves each have concrete versions of those functions.
13:41 < dmwit> yingw787: Both forms of polymorphism -- generics, and object-oriented-like -- are available, via different mechanisms.
13:41 < dmwit> yingw787: Typeclasses are a bit like Java interfaces, and is what ChaiTRex is telling you about. But I suspect what your
               book is talking about at the moment is parametric polymorphism, where no typeclass constraints appear.
13:42 < dmwit> For example,
13:42 < dmwit> :t length
13:42 < lambdabot> Foldable t => t a -> Int
13:42 < dmwit> well
13:42 < dmwit> It used to be that `length :: [a] -> Int` and then it was a better example for what I'm talking about. =P
13:42 < dmwit> Let's suppose `length :: [a] -> Int` for now.
13:43 < yingw787> dmwit: Okay cool, yes this foldable stuff is a bit confusing, but please continue :)
13:43 < dmwit> Since `a` as a type variable is completely unconstrained, this is a lot like generics: `length` works on any list, no
               matter what type of elements are inside.
13:43 < dmwit> Compare, say, (+)
13:43 < dmwit> :t (+)
13:43 < lambdabot> Num a => a -> a -> a
... (DID NOT REALIZE THAT `irssi` WOULD PREVENT SCROLLBACK (O.O') )
```

Dictionary passing implementation of typeclasses:
https://stackoverflow.com/q/38130985

```irc
13:55 < dmwit> Sort of a trivial example of the kind of error you can expect.
13:55 < dmwit> or, for your first question...
13:56 < dmwit> > ('a' :: Char) + ('b' :: Char)
13:56 < lambdabot>  error:
13:56 < lambdabot>      • No instance for (Num Char) arising from a use of ‘+’
13:56 < lambdabot>      • In the expression: ('a' :: Char) + ('b' :: Char)
13:56 < EvanR> yingw787: have you heard of 'dictionary passing' implementation of type classes
13:57 < EvanR> or dictionary passing intuition
13:57 < yingw787> EvanR: I haven't heard about 'dictionary passing', please enlighten me :D
13:58 < EvanR> a constraint on a function can be viewed as a normal argument where the thing passed in at runtime is a dictionary with
               the method implementations in it
13:58 < EvanR> since the programmer doesn't explicitly pass this around, it's the job of the compiler to arrange for where to get this
               dictionary from
13:58 < EvanR> it can come from many places, but if it can't find any place it's a compile time error
13:59 < EvanR> or if it is clearly ambiguous where to get it
13:59 < EvanR> that would be a compile time error
14:00 < boxscape> yingw787 `data Eq a = Eq {(==) :: a -> a -> Bool}`, then instead of having a function like `f :: Eq a => a -> a -> Int`
                  it would be `f :: Eq a -> a -> a -> Int`
14:01 < EvanR> i think this picture goes a good way toward explaining some of the type class related error messages
```

(So, Haskell polymorphism is more like Java interfaces. At run-time, the Haskell
runtime will schedule which concrete method should accept the incoming data. But
the polymorphic method definition is not run directly. If there isn't a matching
method for base types, there will generally be a compile-time error. Constraints
are not types, they are typeclasses.)

- Haskell has *parametric polymorphism* and *constrained (ad-hoc) polymorphism*.
    - Object-oriented polymorphism is akin to ad-hoc polymorphism, and
      implemented in typeclasses.
    - Parametric polymorphism refers to type variables (parameters) that are
      fully polymorphic.
    - Lowercase variables are type variables, uppercase variables are types.

- A type can inherit operation from its superclass via typeclass inheritance
  (e.g. `Int` can inherit operations from `Num`).
- **If a variable can be anything, then there's little that can be don to it
  because it has no methods.**

- **Parametricity**: The behavior of a function w.r.t. the types of its
  parametrically polymorphic arguments is uniform. The behavior cannot change
  because it was applied to an argument of a different type.

********** START EXERCISES: PARAMETRICITY **********

All you can do with a parametrically polymorphic value is pass or not pass it to
some other expression. Prove that to yourself with these small demonstrations.

1. Given the type `a -> a`, which is the type for `id`, attempt to make a
   function that terminates successfully that does something other than
   returning the same value. This is impossible, but you should try it anyway.

2. We can get a more comfortable appreciation of parametricity by looking at `a
   -> a -> a`. This hypothetical function `a -> a -> a` has two -- and only two
   -- implementations. Write both possible versions of a -> a -> a. After doing
   so, try to violate the constraints of parametrically polymorphic values we
   outlined above.

3. Implement `a -> b -> b`. How many implementations can it have? Does the
   behavior change when the types of `a` and `b` change?

********** END EXERCISES: PARAMETRICITY **********