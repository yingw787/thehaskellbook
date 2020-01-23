# Chapter 24

- Parser combinators
    - Parsing: accept serialized input (characters or bytes), and turn it into a
      syntax representation (possibly tree-like)

- A few more words of introduction
    - This chapter covers only what you need; may not be long enough for making
      your own parser (like I want to make a parser).

- Understanding the parsing process
    - Parser: function that takes text input and outputs "structure" as output
    - Parser combinator: higher-order function that takes parser as input and
      returns a new parser as output

    - Oftentimes parser combinators use `Reader`-like implicit argument passing

    - Combinators enable recursion and composition / modularity

- Since we didn't use an analogy for `Monad`
    - See `LearnParsers.hs`.
