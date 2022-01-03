Jacinda is a functional, expression-oriented complement to
[AWK](http://www.awklang.org).

# Further Advantages

  * Arbitrary-precision integers - no overflow
  * [Rust's regular expressions](https://docs.rs/regex/)
    - extensively documented with Unicode support

# PERFORMANCE

## Linux + x64

```
benchmarking bench/ja '(+)|0 {%/Stephen/}{1}' -i /tmp/ulysses.txt
time                 28.70 ms   (28.48 ms .. 29.14 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 29.01 ms   (28.78 ms .. 29.31 ms)
std dev              569.5 μs   (401.2 μs .. 832.8 μs)

benchmarking bench/gawk '/Stephen/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 8.526 ms   (7.801 ms .. 9.627 ms)
                     0.925 R²   (0.869 R² .. 0.972 R²)
mean                 8.093 ms   (7.753 ms .. 8.608 ms)
std dev              1.227 ms   (959.5 μs .. 1.621 ms)
variance introduced by outliers: 75% (severely inflated)

benchmarking bench/mawk '/Stephen/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 6.472 ms   (5.637 ms .. 7.256 ms)
                     0.858 R²   (0.761 R² .. 0.927 R²)
mean                 5.411 ms   (4.821 ms .. 6.014 ms)
std dev              1.792 ms   (1.551 ms .. 2.069 ms)
variance introduced by outliers: 95% (severely inflated)
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
