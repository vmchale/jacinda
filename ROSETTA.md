# [AWK one-liners](https://catonmat.net/blog/wp-content/uploads/2008/09/awk1line.txt)

## Last Line of File

```
ja '[y]|>$0'
```

```awk
awk 'END{print}'
```

## Delete First 6 Lines of File

```
ja '{ix>6}{`0}'
```

```
sed '1,6d'
```

In-place:

```
ja '{ix>6}{`0}' -i FILE | sponge FILE
```

```
sed -i '1,6d' FILE
```

## Word Count

```awk
awk '{ total = total + NF }; END {print total}'
```

```
ja '(+)|0 {|#*`$}'
```

# Get Version

```
git ls-remote https://github.com/musikinformatik/SuperDirt.git | grep tags | tail -n1 | awk -F/ '{print $NF}'
```

```
git ls-remote https://github.com/musikinformatik/SuperDirt.git | ja -F/ '[y]|>{%/tags/}{`*}'
```

# Last Field of First Line

```awk
NR==1{print $NF}
```

```
{ix=1}{`*}
```

# [Print All Disk Space Across Filesystems](https://stackoverflow.com/a/424776)

```awk
df -m | awk '{p+=$3}; END {print p}'
```

```
df -m | ja '(+)|0 {ix>1}{`3:i}'
```

# Print Long Lines with Context

```awk
{ if (length($0) > 180) { printf "%s:%i %s\n", FILENAME, NR, $0 } }
```

```
{#`0>180}{sprintf'%s:%i %s' (fp.ix.`0)}
```

# Filter Users

```
cat /etc/passwd | ja -F: '{`7 !~/false|nologin/}{`1}'
```

```
cat /etc/passwd | awk -F: '$7 !~/false|nologin/ { print $1 }'
```

# Sum Population

```
wget https://burntsushi.net/stuff/worldcitiespop_mil.csv -O /tmp/worldcitiespop_mil.csv
ja -F, '(+)|0 {`5 ~ /^\d+$/}{`5:}' -i /tmp/worldcitiespop_mil.csv
```

```
awk -F, '{ sum += $5 }; END { print sum }' /tmp/worldcitiespop_mil.csv
```

# Deduplicate Lines

As in this [StackOverflow answer](https://unix.stackexchange.com/a/281478).

```awk
!a[$0]++ && ! /^$/
```

```
~.[#x>0] #. $0
```

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

printSpan¨{% / *\^+/}{`2}
```

```
:set fs:=/\|/;

fn printSpan(str) :=
  let
    val p := match str /\^+/
    val str := (sprintf '%i-%i')¨p
  in str end;

printSpan:?{% /\|/}{`2}
```

# Present PATH

```awk
# awk -f path.awk <(echo $PATH)
BEGIN { FS = ":" ;OFS = "\n" }
{$1=$1 ; print $0}
```

```
{. echo $PATH | ja run examples/path.jac
fn path(x) :=
  ([x+'\n'+y])|>(splitc x ':');

path¨$0
```

# Display Running Processes

```
ps u | ja '{ix=1 || `11 ~ /zsh/}{`0}'
```

This is equivalent to `ps u -C zsh` where applicable.

# Ttyplot

In combination with [ttyplot](https://github.com/tenox7/ttyplot):

## Linux

```
sar 1 | gawk '{ print 100-int($NF); fflush(); }' | ttyplot -s 100 -t "cpu usage" -u "%"
```

```
sar 1 | ja ':flush; {ix>3}{100.0-`*:f}' | ttyplot -s 100 -t "cpu usage" -u "%"
```

```
{ while true; do ja '{|`1:f%1000.0}' -i /sys/class/thermal/thermal_zone0/temp; sleep 1; done } | ttyplot -t "cpu temp" -u C
```

```
{ while true; do awk '{ printf("%.1f\n", $1/1000) }' /sys/class/thermal/thermal_zone0/temp; sleep 1; done } | ttyplot -t "cpu temp" -u C
```

## Mac

```
vm_stat 1 | awk '{ print int($2)*4096/1024^3; fflush(); }' | ttyplot -t "MacOS Memory Usage" -u GB
```

```
vm_stat 1 | ja ':flush; {⍳>3}{`2:f*4096.0%(1024.0**3.0)}' | ttyplot -t "MacOS Memory Usage" -u GB
```

```
{ while true; do /System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport --getinfo | awk '/agrCtlRSSI/ {print -$2; fflush();}'; sleep 1; done } | ttyplot -t "wifi signal" -u "-dBm" -s 90
```

```
{ while true; do /System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport --getinfo | ja ':flush; {%/agrCtlRSSI/}{-.`2:i}'; sleep 1; done } | ttyplot -t "wifi signal" -u "-dBm" -s 90
```

# Glibc

```awk
NF == 1 && $1 != "}" {
  haveversion[$1] = 1
}
END {
  for (i in haveversion)
    printf "have-%s = yes\n", i
}
```

```
(sprintf 'have-%s = yes')" ~.{nf=1 & `1 != '}'}{`1}
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
ls -l | ja '(+)|0 {ix>1}{`5:}'
```

# Discard First Line

Posed on [StackOverflow](https://stackoverflow.com/a/34504648):

```awk
NR>1
```

```
{ix>1}{`0}
```

# Print 7th Line of File

From [StackOverflow](https://stackoverflow.com/a/6022441/11296354):

```
sed -n 7p FILE
```

```
awk 'NR == 7' FILE
```

```
ja '{⍳=7}{`0}' -i FILE
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

# Get Version

```
curl https://www.python.org/downloads/ | rg 'Python-(\d(\.\d+)*).tar.xz' -o -r '$1' | head -n1
```

```
curl -s https://www.python.org/downloads/ | ja '[:|>.?{|`0 ~* 1 /Python-(\d(\.\d+)*).tar.xz/}'
```

`[:|>` selects the first line in a stream by folding with the constant function `[:`.

```
curl -s https://www.python.org/downloads/ | ja '[:|>[x ~* 1 /Python-(\d(\.\d+)*).tar.xz/]:?$0'
```

This is perhaps not worth the loss in portability but it shows functional programming doing the heavy lifting (instead of toggling command-line flags).

# Categorize

```
curl https://www.stats.govt.nz/assets/Uploads/Food-price-index/Food-price-index-September-2023/Download-data/food-price-index-september-2023-weighted-average-prices.csv -o food-prices.csv
ja --csv '~.{ix>1}{`8} -i food-prices.csv
```

# StackOverflow

From [this answer](https://unix.stackexchange.com/a/449459):

```awk
awk -F'[/:]+' '{ sub("^www\.", "", $2); print $2 }'
```

```
ja -F'[/:]+' "{|sub1 /^www\./ '' \`2}"
```
