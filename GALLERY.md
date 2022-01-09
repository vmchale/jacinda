# Count lines of code

```
fd '\.(c|h)$' -x wc -l | ja '(+)|0 $1:i'
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

# Count Bytes in Directory

```
ls -l | ja '(+)|0 {ix>1}{`5:i}'
```

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
{|ix}
```
