# Chapter 1

- Referential transparency is critical to making logic composable.

- Lambda calculus has three basic components:
    - Expressions: variable name, abstraction, or combination of those things.
    - Variables: Names for potential inputs to functions.
    - Abstractions: Function, that has a head (lambda), a body, and is applied
      to an argument.

      Basic lambda method looks like: λx.x

- Alpha equivalence: Variable name is not semantically meaningful; λx.x == λd.d

- Beta reduction: Applying the function to an argument, the argument is applied
  to instances of bound variables within the expression, and the lambda head is
  removed; i.e. λx.x(2) -> 2

- You can pass in a method as an argument to a lambda method, for example:
  (λx.x)(λy.y) -> λy.y

- Applications in lambda calculus are left associative, as in they group towards
  the left (reduce towards the right).

  (λx.x)(λy.y)(z) -> ((λx.x)(λy.y))z -> (λy.y)(z) -> z

- Beta reduction stops when there are no longer unevaluated functions applied to
  arguments.

- A computation = initial lambda expression + deduction of lambda terms by beta
  reduction.

- Free variables: Variables in lambda expression body that are not named in the
  head.

  (λx.xy)(z) -> (λ[x:=z].xy) -> zy

- Alpha equivalence does not apply to free variables since free variables may be
  different things, but lambda expressions without modification to free
  variables are alpha equivalent.

- Currying: sequence of lambda applications to multiple heads.

  (λxy.xy) == λx.(λy.xy)

- Currying and beta reduction may look like this:

  1. (λxyz.xz(yz))(λmn.m)(λp.p)
  2. (λx.λy.λz.xz(yz))(λm.λn.m)(λp.p) // Making the currying explicit
  3. (λy.λz.(λm.λn.m)z(yz))(λp.p) // Binding to the outermost lambda, which
     binds first argument to x.
  4. λz.(λm.λn.m)(z)((λp.p)z) // Replaced y with the next argument, which is the
     identity method. Last argument is irreductible because there are no more
     arguments to bind to.
  5. λz.(λn.z)((λp.p)z) // Apply lambda binding m to argument z.
  5.1. λz.(λn.z)(λz.z)
  6. λz.z // reducing the reducible term (λn.z) with the argument of the
     identity method, which becomes z.

Intermission: Equivalence exercises

1. λxy.xz == b) λmn.mz (replacement of x with z due to alpha equivalence, w/o
   touching free variable z)
2. λxy.xxy == c) λa.(λb.aab) (by currying).
3. λxyz.zx == b) λtos.st (by alpha equivalence).

Normal form (usually means) Beta normal form:
- Cannot beta reduce (apply lambdas to arguments) the terms any further.
- Corresponds to a fully evaluated expression, or a fully executed program.
- Saying "2" is easier than saying "2000 / 1000" (a function fully applied to
  two arguments, but not reduced).
- Can either be a variable with no lambda, or a lambda with no variable.

Combinator: Lambda term with no free variables.
- Serve only to combine the arguments they are given.
- Every term in the body occurs in the head.

- λx.x (IS COMBINATOR, bound by enclosing lambda)
- λxy.x (IS COMBINATOR, expression variables subset of enclosing lambda)
- λxyz.xz(yz) (IS COMBINATOR, doesn't have to be reduced, reduced form is likely
  λyz.yzz, arguments are present in enclosing lambda)

- λy.x (IS NOT COMBINATOR, x is free)
- λx.xz (IS NOT COMBINATOR, z is free)

Divergence: Reducible lambdas that do not converge to beta normal form.

- (λx.xx)(λx.xx)
- ([x:=(λx.xx)]xx) // Substituting (λx.xx) for each x in enclosing lambda
- (λx.xx)(λx.xx) // Back to where we started

Terms that diverge don't produce an answer or meaningful result.

Chapter Exercises:

1. Combinators: Determine if each of the following functions are combinators or not.

- λx.xxx (IS COMBINATOR, bound by enclosing lambda)
- λxy.zx (IS NOT COMBINATOR, z is free)
- λxyz.xy(zx) (IS NOT COMBINATOR, beta normal form is λyz.zxy where x is a free
  variable).
- λxyz.xy(zxy) -> λyz.(zxy)(y) (IS NOT COMBINATOR, beta normal form has x as a
  free variable).
- λxy.xy(zxy) -> λy.(zxy)(y) (IS NOT COMBINATOR, x and z are free variables).

2. Normal form or diverge? Determine if each of the following expressions can be
   reduced to a normal form or if they diverge.

- λx.xxx (NORMAL FORM, lambda without arguments cannot be reduced further)
- (λz.zz)(λy.yy) (DIVERGENT, alpha equivalence equal to divergent example
  earlier in book)
- (λx.xxx)(z) -> zzz (NORMAL FORM, fully applied and can be reduced to beta
  normal form)

3. Beta reduce; Evaluate (that is, beta reduce) each of the following
   expressions to normal form. We strongly recommend writing out the steps on
   paper with a pencil or pen.
