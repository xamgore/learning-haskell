import Criterion.Main
import Task3

main = defaultMain [
        bgroup "f1" [
            bench "1"          $ whnf f1 [1..1],
            bench "1000"       $ whnf f1 [1..1000],
            bench "1000000"    $ whnf f1 [1..1000000],
            bench "1000000000" $ whnf f1 [1..1000000000]
        ] ]

{-
Running 1 benchmarks...
Benchmark lab2-bench: RUNNING...
benchmarking f1/1
time                 31.13 ns   (30.99 ns .. 31.35 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 31.12 ns   (31.02 ns .. 31.34 ns)
std dev              462.7 ps   (125.4 ps .. 913.0 ps)
variance introduced by outliers: 19% (moderately inflated)

benchmarking f1/1000
time                 4.637 μs   (4.592 μs .. 4.722 μs)
                     0.995 R²   (0.988 R² .. 1.000 R²)
mean                 4.799 μs   (4.656 μs .. 5.286 μs)
std dev              755.4 ns   (286.7 ns .. 1.555 μs)
variance introduced by outliers: 95% (severely inflated)

benchmarking f1/1000000
time                 5.848 ms   (5.761 ms .. 5.956 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 5.786 ms   (5.757 ms .. 5.832 ms)
std dev              110.2 μs   (68.07 μs .. 177.1 μs)

benchmarking f1/1000000000
Progress: 1/2^Cuser interrupt-}
