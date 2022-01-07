Jacinda is a functional, expression-oriented complement to
[AWK](http://www.awklang.org).

# Installation

## From Source

First, install [Rust's regex library](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine).

If you have [cabal](https://www.haskell.org/cabal/) and [GHC](https://www.haskell.org/ghc/) installed (via [ghcup](https://www.haskell.org/ghcup/)):

```
cabal install jacinda
```

# Documentation

The manpages include various builtins and a syntax reference.

# Further Advantages

  * [Rust's regular expressions](https://docs.rs/regex/)
    - extensively documented with Unicode support

# PERFORMANCE

## Linux + x64

```
benchmarking bench/ja '(+)|0 {%/Bloom/}{1}' -i /tmp/ulysses.txt
time                 15.33 ms   (15.21 ms .. 15.56 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 15.52 ms   (15.44 ms .. 15.63 ms)
std dev              246.5 μs   (203.2 μs .. 297.6 μs)

benchmarking bench/original-awk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 13.18 ms   (13.10 ms .. 13.29 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 13.06 ms   (13.01 ms .. 13.12 ms)
std dev              147.8 μs   (119.1 μs .. 201.4 μs)

benchmarking bench/gawk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 7.281 ms   (7.213 ms .. 7.370 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 7.331 ms   (7.284 ms .. 7.408 ms)
std dev              165.9 μs   (105.0 μs .. 232.6 μs)

benchmarking bench/mawk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 2.912 ms   (2.736 ms .. 3.059 ms)
                     0.927 R²   (0.841 R² .. 0.980 R²)
mean                 3.350 ms   (3.100 ms .. 3.758 ms)
std dev              1.023 ms   (588.7 μs .. 1.496 ms)
variance introduced by outliers: 96% (severely inflated)

benchmarking bench/busybox awk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 12.68 ms   (12.46 ms .. 13.08 ms)
                     0.994 R²   (0.982 R² .. 1.000 R²)
mean                 12.95 ms   (12.74 ms .. 13.28 ms)
std dev              682.5 μs   (371.1 μs .. 1.020 ms)
variance introduced by outliers: 25% (moderately inflated)
```

## OSX + Aarch64

```
benchmarking bench/ja '(+)|0 {%/Bloom/}{1}' -i /tmp/ulysses.txt
time                 27.95 ms   (25.60 ms .. 31.51 ms)
                     0.941 R²   (0.894 R² .. 0.987 R²)
mean                 25.45 ms   (23.95 ms .. 27.05 ms)
std dev              3.604 ms   (2.496 ms .. 5.447 ms)
variance introduced by outliers: 62% (severely inflated)

benchmarking bench/awk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 37.09 ms   (34.59 ms .. 39.64 ms)
                     0.990 R²   (0.983 R² .. 0.999 R²)
mean                 40.35 ms   (37.41 ms .. 51.32 ms)
std dev              10.69 ms   (931.2 μs .. 20.31 ms)
variance introduced by outliers: 86% (severely inflated)
```
