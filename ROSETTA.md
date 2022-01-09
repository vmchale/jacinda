# Count Lines

```awk
{ total += 1 }; END { print total }
```

```
(+)|0 {#t}{1}
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

```
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

```
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
