# Chapter 4

- Basic datatypes

- Types are like sets (set theory -> type theory), disjunction (or) /
  conjunction (and)
- Data declarations: Defining new datatypes.
- Data constructors: Values that inhabit the type they are defined in.
- Term level: Values as they appear in *your* code.

```haskell
Prelude> data Bool = False | True
Prelude> :info Bool
data Bool = False | True 	-- Defined at <interactive>:1:1
Prelude>
```

```haskell
-- NOTE: Must not redefine Prelude.Bool for 'not' to work.
Prelude> :t not
not :: Bool -> Bool
Prelude> not True
False
Prelude>
```

********** START EXERCISES: MOOD SWING **********

Given the following datatype, answering the following questions:

```haskell
data Mood = Blah | Woot deriving Show
```

1.  What is the type constructor, or name of this type?

    `Mood`.

2.  If the function requires a `Mood` value, what are the values you could
    possibly use?

    `Blah` or `Woot deriving Show`.

3.  We are trying to write a function `changeMood` to change Chris's mood
    instantaneously. It should act like `not` in that, given one value, it
    returns the *other* value of the same type. So far, we've written a type
    signature `changeMood :: Mood -> Woot`. What's wrong with that?

    Method signature should be `changeMood :: Mood -> Mood`, as both `Blah` and
    `Woot` both are of type `Mood`.

4.  Now we want to write the function that changes his mood. Given an input
    mood, it gives us the other one. Fix any mistakes and complete the function:

    ```haskell
    -- pattern matching
    changeMood Mood = Woot
    changeMood _ = Blah
    ```

    Change the input argument resulting in value `Woot` to have input `Blah`.

    ```haskell
    changeMood Blah = Woot
    changeMood _ = Blah
    ```

5.  Enter all of the above -- datatype (including the `deriving Show` bit), your
    corrected type signature, and the corrected function into a source file.
    Load it and run it in GHCi to make sure you got it right.

    ```haskell
    Prelude> data Mood = Blah | Woot deriving Show
    Prelude> :{
    Prelude| changeMood :: Mood -> Mood
    Prelude| changeMood Blah = Woot
    Prelude| changeMood _ = Blah
    Prelude| :}
    Prelude> changeMood Woot
    Blah
    Prelude> changeMood Blah
    Woot
    ```

********** END EXERCISES: MOOD SWING **********
