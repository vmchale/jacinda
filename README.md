Jacinda is a functional pattern sifting language,
a smaller [AWK](http://www.awklang.org).

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

# Usefulness

Unix uses record separators in many places; we can display one entry in the
`PATH` variable with:

```
echo $PATH | ja -F: "{|[x+'\n'+y]|>\`$}"
```

Many Unix tools output much information separated with spaces. We use regular
expressions to match relevant lines and then select the field with the data
itself, viz.

```
otool -l $(locate libpng.dylib) | ja '{`1 ~ /^name/}{`2}'
```

To get the value of a variable (say, `PATH`) from the output of `printenv`:

```
printenv | ja -F= '{%/^PATH/}{`2}'
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
  * Streams of functions don't work
  * Higher-order functions are subtly broken

Intentionally missing features:

  * No loops

# Advantages

  * [Rust's regular expressions](https://docs.rs/regex/)
    - extensively documented with Unicode support
  * Deduplicate builtin

# Contributing

I have rewritten the code several times so forking and applying patches is fraught!

Bug reports are welcome contributions.
