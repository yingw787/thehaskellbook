# Chapter 28

- Basic libraries and data structures
    - Choosing the right data structures is critical to performance and
      performance optimization in Haskell

- Benchmarking with Criterion
    - Can sample performance using Monte Carlo method
    - [`criterion`](http://hackage.haskell.org/package/criterion): Haskell
      performance analysis and measurement
    - While compiling code for benchmarking, use `-O` or `-O2` in build flags to
      GHC
        - `stack ghc -- -O2 bench.hs`

- See `Bench.hs`.

```haskell
Prelude> :l Bench.hs
[1 of 1] Compiling Bench            ( Bench.hs, interpreted )
Ok, one module loaded.
*Bench> main
benchmarking index list 9999
time                 19.30 μs   (18.96 μs .. 19.70 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 19.11 μs   (18.93 μs .. 19.39 μs)
std dev              743.4 ns   (441.2 ns .. 1.238 μs)
variance introduced by outliers: 46% (moderately inflated)

benchmarking index list maybe index 9999
time                 5.906 ms   (5.778 ms .. 6.038 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 5.796 ms   (5.745 ms .. 5.873 ms)
std dev              192.4 μs   (131.0 μs .. 286.3 μs)
variance introduced by outliers: 15% (moderately inflated)
```
