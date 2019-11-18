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
