Jacinda is a functional, expression-oriented complement to
[AWK](http://www.awklang.org).

# SHOCK & AWE

```
ls -l | ja '(+)|0 {ix>1}{`5:i}'
```

```
curl -sL https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-weekly-breakthrough.csv | ja ',[1.0-x%y] {ix>1}{`5:f} {ix>1}{`11:f}' -F,
```

# Installation

## From Source

First, install [Rust's regex library](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine).

If you have [cabal](https://www.haskell.org/cabal/) and [GHC](https://www.haskell.org/ghc/) installed (via [ghcup](https://www.haskell.org/ghcup/)):

```
cabal install jacinda
```

# Documentation

The manpages document the builtins and a syntax reference.

# Further Advantages

  * [Rust's regular expressions](https://docs.rs/regex/)
    - extensively documented with Unicode support

# PERFORMANCE

## Linux + x64

```
benchmarking bench/ja '(+)|0 {%/Bloom/}{1}' -i /tmp/ulysses.txt
time                 8.110 ms   (7.926 ms .. 8.304 ms)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 8.470 ms   (8.278 ms .. 8.771 ms)
std dev              693.0 μs   (437.4 μs .. 1.008 ms)
variance introduced by outliers: 47% (moderately inflated)

benchmarking bench/original-awk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 13.24 ms   (13.04 ms .. 13.39 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 13.39 ms   (13.29 ms .. 13.49 ms)
std dev              256.0 μs   (197.8 μs .. 380.7 μs)

benchmarking bench/gawk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 7.804 ms   (7.706 ms .. 7.931 ms)
                     0.996 R²   (0.991 R² .. 0.999 R²)
mean                 7.668 ms   (7.572 ms .. 7.783 ms)
std dev              303.4 μs   (229.7 μs .. 442.5 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking bench/mawk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 3.179 ms   (3.099 ms .. 3.240 ms)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 3.213 ms   (3.178 ms .. 3.270 ms)
std dev              148.9 μs   (97.11 μs .. 267.6 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking bench/busybox awk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 12.61 ms   (12.43 ms .. 12.77 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 12.98 ms   (12.86 ms .. 13.09 ms)
std dev              303.1 μs   (234.5 μs .. 396.2 μs)
```
