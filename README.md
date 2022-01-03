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
time                 35.97 ms   (33.98 ms .. 38.48 ms)
                     0.988 R²   (0.978 R² .. 0.995 R²)
mean                 33.72 ms   (32.46 ms .. 34.73 ms)
std dev              2.342 ms   (1.874 ms .. 2.878 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking bench/awk '/Bloom/ { total += 1; } END { print total }' /tmp/ulysses.txt
time                 56.48 ms   (56.03 ms .. 57.02 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 56.15 ms   (56.04 ms .. 56.38 ms)
std dev              282.1 μs   (127.9 μs .. 465.7 μs)
```
