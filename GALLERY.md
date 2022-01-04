# Count lines of code

(Haskell + Alex + Happy)

```
fd '\.(hs|x|y)$' -x wc -l | ja '(+)|0 $1:i'
```

(C + Yacc)

```
fd '\.(c|h|y)$' -x wc -l | ja '(+)|0 $1:i'
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
(+)|0 {#t}{1}
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
(+)|0 {#t}{#`0}
```

# Count Occurrences

```
(+)|0 {%/Bloom/}{1}
```

# Longest Line

```
max|_1 #"$0
```

All lines >110 bytes:

```
{#`0>110}{`0}'
```

Is there a line >110 bytes?

```
(||)|#f {#`0>110}{#t}
```

```
(max|_1 #"$0) > 110
```

# CSV Processing

```
csvformat excess-j.csv -D '|' | ja ':set fs := /\|/; $1'
```

# Balance Parens

```
let
  val count := [(+)|0 x]
  val lparen := {%/\(/}{1}
  val rparen := {%/\)/}{1}
in count lparen = count rparen end
```

# Count Bytes in Directory

```
ls -l | ja '(+)|0 {ix>1}{`5:i}'
```

# In Conjunction with awk

```
cat *.pgn | grep "Result" | awk '{ split($0, a, "-"); res = substr(a[1], length(a[1]), 1); if (res == 1) white++; if (res == 0) black++; if (res == 2) draw++;} END { print white+black+draw, white, black, draw }'
```

```
cat *.pgn | grep "Result" | awk '{ split($0, a, "-"); res = substr(a[1], length(a[1]), 1); print res }' | ja 'let val sum := [(+)|0 x] in (sum {`1~/\d/ & `1:i=1}{1}) end'
```

Awk is doing the heavy lifting here but it is notably more prolix when
summarizing data.

# Library Functions

One could write

```
sum := [(+)|0 x]

product := [(*)|1 x]

any := [(||)|#f x]

all := [(&)|#t x]

count := [(+)|0 [:1"x]

maximum := [max|_1 x]
```

# Print Line Number

```
(+)^0 [:1"$0
```

```
{#t}{ix}
```
