# All Compiler Instructions

Improvement on a [shell one-liner](http://pepijndevos.nl/2016/08/24/x86-instruction-distribution.html)

```
objdump -d /usr/bin/* | cut -f3 | ja '~.{%/^[a-z]+/}{`1}'
```

# All packages on Hackage

```
cabal list --simple | ja '~.$1'
```

(one can download all packages from Hackage with:)

```
cabal list --simple | ja '~.$1' | xargs cabal get
```

This is simpler than:

```
cabal list --simple | cut -d' ' -f1 | sort -u
```

# Trim URL

```
echo 'https://soundcloud.com/shitzulover07/ayesha-erotica-vacation-bible-school?utm_medium=text&utm_campaign=social_sharing' \
    | ja '.?{|`1 ~* 1 /([^\?]*)/}'
```

yields

```
https://soundcloud.com/shitzulover07/ayesha-erotica-vacation-bible-school
```

## Mac

```
pbpaste | ja '.?{|`1 ~* 1 /([^\?]*)/}' | pbcopy
```

# Display Name of Terminal

Modification of a [StackExchange](https://askubuntu.com/a/476663) one-liner
using Awk.

```
ps -aux | rg "$(ps -p $$ -o ppid=)" | ja '{ix=1}{`*}'
```

# Show All Lines Introduced in a Diff >80 Characters

```
git diff origin/master | ja '[#x>81]#.{%/^\+/}{`0}'
```

Note the `81` to account for the leading `+`.

# Find all distinct language extensions used in a project

```
fd '\.hs$' . -x ja '.?{|`0 ~* 1 /LANGUAGE\s*([a-zA-Z]*)\s*#-/}' -i | ja '~.$0'
```

One can make this more rigorous (allowing for `{-# LANGUAGE Xxx,Yyy #-}` with:

```
@include'lib/string.jac'

fn findExtensions(line) :=
  let
    val extStr ≔ line ~* 1 /\{-#\s*LANGUAGE\s*([^\s]*)\s*#-\}/
    val extList ≔ (\s.split s /,\s*/)"extStr
  in extList end;

~.(\x.(intercalate'\n')"(findExtensions x)):?$0
```

Which we can invoke with:

```
fd '\.hs$' . -x ja run hsExtensions.jac -i | ja '~.$0'
```

This can be used to populate the `other-extensions` field in a `.cabal` file.

# cdc

```
curl -sL https://data.cdc.gov/api/views/9mfq-cb36/rows.csv | sort | csvformat -D'|' | ja -F'\|' ',[(x.y)] ((-)\. {%/2021.*TX/}{`3:i}) {%/2021.*TX/}{`1}'
```

# NYC Vaccine Effectiveness

```
curl -sL https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-weekly-breakthrough.csv | \
    ja ',[1.0-x%y] {ix>1}{`5:} {ix>1}{`17:}' -F,
```

# Ctags

```
ja -F= '{%/let *[[:lower:]][[:alnum:]]*/}{(⍳.`1)}' -i /development/dhall/dhall-kitty/conf.dhall
```

# Sum Total Population

```
wget https://burntsushi.net/stuff/worldcitiespop_mil.csv -O /tmp/worldcitiespop_mil.csv
ja -F, '(+)|0 {`5 ~ /\d+/}{`5:}' -i /tmp/worldcitiespop_mil.csv
```
