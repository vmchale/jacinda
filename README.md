Jacinda is a functional, expression-oriented complement to
[AWK](http://www.awklang.org).

# Further Advantages

  * Arbitrary-precision integers - no overflow
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
