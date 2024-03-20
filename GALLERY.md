# Extract Library Versions

```
strings -d $(which ja) | ja '~.[x ~* 1 /(^[A-Za-z][A-Za-z0-9\-]*\-\d+(\.\d+)*)\-([0-9a-f]{64}$|[0-9a-f]{4})/]:? $0'
strings -d $(which ja) | ja '~..?{| `0 ~* 1 /(^[A-Za-z][A-Za-z0-9\-]*\-\d+(\.\d+)*)\-([0-9a-f]{64}|[0-9a-f]{4})/}'
```

## Extract Library Versions (Unstripped)

```
@include'lib/string.jac'

unlines¨{% /-lHS/}{captures `0 1 /-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)/}
```

```
readelf -p '.debug-ghc-link-info' $(which pandoc) | ja -R, '.?{|`0 ~* 1 /-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)/}'
```

```
readelf -p '.debug-ghc-link-info' $(which pandoc) | tr ',' '\n' | rg '\-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)' -o -r'$1'
```

# Show Dynamic Library Dependencies

```
readelf -d $(which vim) | ja '.?{%/Shared library/}{`5 ~* 1 /\[(.*)\]/}'
```

# Show Dynamic Library Dependencies (Mac)

```
otool -l $(locate libpng.dylib | tail -n1) | ja -R'Load command' '{%/LC_LOAD_DYLIB/}{`7}'
```

# Present RUNPATH (ELF)

```
readelf -d libapple.so | \
    ja '.?{%/RUNPATH/}{`5 ~* 1 /\[(.*)\]/}' | \
    tr ':' '\n'
```

# Show Machine Architecture (ELF)

```
readelf -h $(locate libpng.so | tail -n1) | ja -F':\s*' '{%/Machine/}{`2}'
```

# Tag Releases

Use the following to create a git tag by extracting the current version number
from the `.cabal` file:

```
git tag "$(ja -F'\s*:\s*' '{%/^\s*version/}{`2}' -i jacinda.cabal)"
```

# Imitate killall

To kill all running GHC processes:

```
kill $(ps u | ja '{%/ghc/}{`2}')
```

# Format All Code In a Haskell Project

```
fd '\.hs$' $(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i jacinda.cabal) -x stylish-haskell -i
```

# Count lines of code

```
fd '\.(c|h)$' -x wc -l | ja '(+)|0 $1:i'
```

# Linecount

Count nonblank lines:

```
(+)|0 {%/./}{1}
```

Count blank lines:

```
(+)|0 {%/^$/}{1}
```

# Count Occurrences

```
(+)|0 {%/Bloom/}{1}
```

# Longest Line

```
max|_1 #¨$0
```

All lines >110 bytes:

```
{#`0>110}{`0}'
```

```
[#x>110] #. $0
```

Find all Haskell code >80 bytes wide

```
fd '\.hs$' -x ja '[#x>80] #. $0' -i
```

Is there a line >110 bytes?

```
(||)|#f {#`0>110}{#t}
```

```
(max|_1 #¨$0) > 110
```

# Prelude

One could write

```
sum := [(+)|0 x]

product := [(*)|1 x]

or := [(||)|#f x]

and := [(&)|#t x]

count := [(+)|0 [:1"x]

maximum := [max|_1 x]
```

# Print Line Number

```
(+)^0 [:1"$0
```

```
{|⍳}
```
