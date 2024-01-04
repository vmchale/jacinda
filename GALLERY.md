# Extract Library Versions

```
strings $(which ja) | ja '~..?{| `0 ~* 1 /(^[A-Za-z][A-Za-z0-9\-]*\-\d+(\.\d+)*)\-[0-9a-f]{64}$/}'
strings $(which ja) | ja '~.[x ~* 1 /(^[A-Za-z][A-Za-z0-9\-]*\-\d+(\.\d+)*)\-[0-9a-f]{64}$/]:? $0'
```

## Extract Library Versions (Unstripped)

```
@include'lib/string.jac'

(+)|'' (intercalate '\n')¨{% /-lHS/}{captures `0 1 /-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)/}
```

# Get Library Versions

```
cabal-plan dot | ja '~.{%/"/}{`1}'
```

# Imitate killall

To kill all running GHC processes:

```
kill $(ps aux | ja "[x+' '+y]|>{%/ghc/}{\`2}")
```

# Format All Code In a Haskell Project

```
fd '\.hs$' $(ja "[x+' '+y]|>{%/hs-source-dirs/}{\`2}" -i jacinda.cabal) -x stylish-haskell -i
```

More rigorously:

```
fd '\.hs$' $(ja '.?{|`0 ~* 1 /^\s*hs-source-dirs:\s*(.*)/}' -i jacinda.cabal) -x stylish-haskell -i
```

# Count lines of code

```
fd '\.(c|h)$' -x wc -l | ja '(+)|0 $1:i'
```

# Long Lines in Source Code

```
fd '\.hs$' -x ja "{|sprintf '%i %s:%i %s' (#\`0.fp.ix.\`0)}" -i | sort -n
```

# Label lines

```
(+)^0 [:1"$0
```

# Imitate fd

```
ls -1 -R | ja '{% /\.hs$/}{`0}'
```

(Find all Haskell source files in a directory)

# Linecount

```
(+)|0 [:1"$0
```

```
(+)|0 {|1}
```

Count nonblank lines:

```
(+)|0 {%/./}{1}
```

Count blank lines:

```
(+)|0 {%/^$/}{1}
```

# Bytecount

```
(+)|0 #"$0
```

```
(+)|0 {|#`0}
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

# Count Bytes in Directory

```
ls -l | ja '(+)|0 {ix>1}{`5:}'
```

# Library Functions

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
