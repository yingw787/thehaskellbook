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

- (λabc.cba)zz(λwv.w)
--> (λabc.cba)(zz)(λwv.w) // wrapping zz in parentheses
--> (λbc.cb(zz))(λwv.w) // left associativity, substituting first argument for
first variable in first lambda.
--> (λc.c(λwv.w)(zz)) // left associativity, substituting second argument for
second variable in first lambda.
--> (λc.c(λv.zz)) // leftmost lambda cannot be reduced, moving onto second
lambda, substituting first argument for first variable in second lambda.
--> λv.zz (beta normal form) // identity lambda results in argument being beta
normal form.

- (λx.λy.xyy)(λa.a)b
--> (λxy.xyy)(λa.a)(b) // uncurrying first lambda, wrapping last argument in
parentheses.
--> (λy.(λa.a)yy)(b) // left associativity, substituting first argument for
first variable in first lambda.
--> (λy.yy)(b) // first lambda is irreducible, left associating second lambda in
expression.
--> bb // substituting argument for lambda variable, beta normal form.

- (λy.y)(λx.xx)(λz.zq)
--> (λx.xx)(λz.zq) // Identity function can be removed by left associativity.
--> (λz.zq)(λz.zq) // Left associativity, substituting second lambda for each x
in first lambda.
--> (λz.zq)(q) // Left associativity, substituting second lambda for z in first
lambda.
--> qq // Substituting q for z in first lambda, not sure if it should be q * q_0
though

- (λz.z)(λz.zz)(λz.zy) // hint: alpha equivalence
--> (λz0.z0)(λz1.z1z1)(λz2.z2y) // renaming variables for deconfliction
--> (λz1.z1z1)(λz2.z2y) // removing identity lambda by left associativity
--> (λz2.z2y)(λz2.z2y) // left associativity, substituting second lambda for z1.
--> (λz2.z2y)(y) // left associativity, substituting second lambda for z2.
--> yy // left associativity, substituting y for z2.

- (λx.λy.xyy)(λy.y)y
--> (λx.λy0.xy0y0)(λy1.y1)(y2) // renaming variables for deconfliction
--> (λxy0.xy0y0)(λy1.y1)(λy2) // de-currying
--> (λy0.(λy1.y1)(y0))(y2) // substituting identity method as x
--> (λy0.y0)(y2) // substituting y0 for y1 in second lambda
--> y2 // substituting y2 for y0, beta normal form.

- (λa.aa)(λb.ba)c
--> (λa0.a0a0)(λb.ba1)(c) // renaming variables for deconfliction
--> (λb.ba1)(λb.ba1)(c) // substituting second lambda for a in first lambda.
--> (λb.ba1)(a1)(c) // substituting second lambda for b in first lambda.
--> (a1a1c) // substituting a1 for b in first lambda, beta normal form as no
more lambdas to apply for arguments.

- (λxyz.xz(yz))(λx.z)(λx.a)
--> (λx0y0z0.x0z0(y0z0))(λx1.z1)(λx2.a0) // renaming variables for
deconfliction; (yz) --> (y0z0) because lack of second head means same context
--> (λy0z0.(y0z0z0)(λx1.z1))(λx2.a0) // substituting second lambda for x0 in
first lambda
--> (λz0.(z0)(z0)(λx1.z1))(λx2.a0) // substituting second lambda for y0 in
first lambda.
--> (λx1.z1)(λx1.z1)(λx2.a0) // substituting second lambda for z0 in first
lambda.
--> z1(λx2.a0) // substituting second lambda for x1 in first lambda.
--> (λx2.a0)(z1) // by commutativity.
--> a0 // substituting z1 for x2 in first lambda, beta normal form.

ANSWERS:

Eqivalence exercises:

1. CORRECT
2. CORRECT
3. CORRECT

Combinators:

1. CORRECT
2. CORRECT
3. INCORRECT (λxyz.xy(xx)) (Is a combinator, none of the arguments in the head
   have been applied, so it's irreducible, and context of one head means alpha
   equivalence does not apply here).
4. INCORRECT (λxyz.xy(zxy)) (Is a combinator, none of the arguments have been
   applied so it's irreducible).
5. CORRECT

Normal form or diverge?

1. CORRECT
2. CORRECT
3. CORRECT

Beta reduce

1. INCORRECT RESULT

- (λabc.cba)zz(λwv.w)
--> (λa.λb.λc.cba)(z)z(λw.λv.w) // currying, not de-currying
--> (λb.λc.cbz)(z)(λw.λv.w) // left associativity
--> (λc.czz)(λw.λv.w)
--> (λw.λv.w)(z)z
--> (λv.z)(z)
--> z

2. CORRECT RESULT, INCORRECT DEDUCTION

- (λx.λy.xyy)(λa.a)b
--> (λy.(λa.a)yy)(b)
--> (λa.a)(b)b // apply from the outside in
--> bb

3. CORRECT RESULT, CORRECT DEDUCTION

4. CORRECT RESULT, CORRECT DEDUCTION

5. INCORRECT RESULT

- (λx.λy.xyy)(λy.y)y
- (λy.(λy.y)yy)(y)
- (λy.y)(y)y // substituting argument y for input y, complete replacement
- yy

6. CORRECT RESULT, CORRECT DEDUCTION

7. INCORRECT RESULT

- (λxyz.xz(yz))(λx.z)(λx.a)
--> (λx.λy.λz.xz(yz))(λx.z)(λx.a) // currying
--> (λy.λz1.(λx.z)z1(yz1))(λx.a) // substituting λx.z for x in first lambda, z
in first lambda renamed to z1.
--> (λz1.(λx.z)(z1)((λx.a)z1)) // substituting λx.a for y in first lambda.
--> (λz1.z((λx.a)(z1))) // left associativity, z1 applied to λx.z --> z
--> (λz1.za) // left associativity, z1 applied to λx.a --> a
