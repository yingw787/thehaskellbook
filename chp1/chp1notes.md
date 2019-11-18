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
