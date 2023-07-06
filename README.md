Jacinda is a functional, expression-oriented data processing language,
complementing [AWK](http://www.awklang.org).

# Installation

## Releases

There are binaries for some platforms on the [releases page](https://github.com/vmchale/jacinda/releases/).

If you are on Mac, you will need to install `*-librure.dylib` as well.

## From Source

First, install [Rust's regex library](https://github.com/rust-lang/regex/tree/master/regex-capi#c-api-for-rusts-regex-engine). You'll need to put `librure.so` or `librure.dylib` etc. in the appropriate place.

If you have [cabal](https://www.haskell.org/cabal/) and [GHC](https://www.haskell.org/ghc/) installed (perhaps via [ghcup](https://www.haskell.org/ghcup/)):

```
cabal install jacinda
```

## Editor Support

There is a [vim plugin](https://github.com/vmchale/jacinda-vim) and a [VSCode extension](https://marketplace.visualstudio.com/items?itemName=vmchale.jacinda).

# SHOCK & AWE

```
curl -sL https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-weekly-breakthrough.csv | \
    ja ',[1.0-x%y] {ix>1}{`5:} {ix>1}{`17:}' -F,
```

## Rosetta

Replace

```awk
NF == 1 && $1 != "}" {
  haveversion[$1] = 1
}
END {
  for (i in haveversion)
    printf "have-%s = yes\n", i
}
```

with

```
(sprintf 'have-%s = yes')" ~.{nf=1 & `1 != '}'}{`1}
```

# Documentation

See the [guide](https://vmchale.github.io/jacinda/), which contains a tutorial
on some of the features as well as examples.

The manpages document the builtins and provide a syntax reference.

# Status

## Missing Features & Bugs

  * No nested dfns
  * No list literal syntax
  * Postfix `:f` and `:i` are handled poorly
  * Polymorphic functions can't be instantiated with separate types (global
    monomorphism restriction)

Intentionally missing features:

  * No loops

# Advantages

  * [Rust's regular expressions](https://docs.rs/regex/)
    - extensively documented with Unicode support
  * Deduplicate builtin
