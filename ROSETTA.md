# Process Compiler Output

```awk
BEGIN { FS="\|" }

/ *\^+/ {
    p=match($2, "\\^+")
    colstart=RSTART-1
    col=colstart+RLENGTH
    printf("%d-%d\n", colstart, col)
}
```

```
:set fs:=/\|/;

fn printSpan(str) :=
  let
    val p := match str /\^+/
    val str := option '(none)' (sprintf '%i-%i') p
  in str end;

printSpan"{% / *\^+/}{`2}
```

# Present PATH

```awk
# e.g. awk -f path.awk <(echo $PATH)
BEGIN { FS = ":" ;OFS = "\n" }
{$1=$1 ; print $0}
```

```
{. echo $PATH | ja run examples/path.jac
fn path(x) :=
  ([x+'\n'+y])|'' (splitc x ':');

path"$0
```

# ?

```awk
BEGIN { calls="" }

{
  calls = calls " " $1;
  print "sysno-" $1 " = " $2;
  print "nargs-" $1 " = " $3;
}

END { print "mach-syscalls := " calls }
```

```
fn printLines(lines) :=
  [x+'\n'+y]|'' lines;

let
  val calls := [x+' '+y]|'' $1
  val prologue := printLines {| sprintf 'sysno-%s = %s\nnargs-%s = %s' (`1 . `2 . `1 . `3)}
in sprintf '%s\nmach-syscalls := %s' (prologue . calls) end
```

# Count Lines

```awk
{ total += 1 }; END { print total }
```

```
(+)|0 {|1}
```

```
(+)|0 [:1"$0
```

## Count Lines Mentioning

```awk
/Bloom/ { total += 1; } END { print total }
```

```
'(+)|0 {%/Bloom/}{1}'
```

## Are There Lines >72 bytes?

```awk
{ if (length($0) > 72) { res = 1; }} END { print res; }
```

```
(||)|#f {#`0>72}{#t}
```

```
(max|_1 #"$0) > 72
```

# Count bytes

```awk
{ total += length($0) }; END { print total }
```

```
(+)|0 #"$0
```

# Sum Directory Content Size

```awk
ls -l | awk '{ total += $5; } END { print total; }'
```

```
ls -l | ja '(+)|0 {ix>1}{`5:i}'
```

# Discard First Line

Posed on [StackOverflow](https://stackoverflow.com/a/34504648):

```awk
NR>1
```

```
{ix>1}{`0}
```

# Data Science

See [here](https://adamdrake.com/command-line-tools-can-be-235x-faster-than-your-hadoop-cluster.html) for an explanation.

```awk
{ split($0, a, "-"); res = substr(a[1], length(a[1]), 1); if (res == 1) white++; if (res == 0) black++; if (res == 2) draw++; } END { print white+black+draw, white, black, draw }
```

```
fn count(x) ≔
  (+)|0 [:1"x;

fn processLine(b) ≔
  let
    val pre := (split b /-/).1
    val l := #pre
    val res := substr pre (l-1) l
  in res end;

let
  val iStream := processLine"$0
  val white := count ((='1') #. iStream)
  val black := count ((='0') #. iStream)
  val draw := count ((='2') #. iStream)
  val total := white + black + draw
in (total . white . black . draw) end
```
