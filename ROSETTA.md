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
/West Virginia/ { total += 1; } END { print total }
```

```
'(+)|0 {%/West Virginia/}{1}'
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
