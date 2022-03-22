# Show All Lines Introduced in a Diff >80 Characters

```
git diff origin/master | ja '[#x>81]#.{%/^\+/}{`0}'
```

Note the `81` to account for the leading `+`.

# Find all distinct language extensions used in a project

```
fd '\.hs$' "$1" -x ja '~.{%/LANGUAGE\s*.*\s*#-/}{`3}' -i | ja '~.$0'
```

One can make this more rigorous (allowing for `{-# LANGUAGE Xxx,Yyy #-}` with:

```
@include'lib/string.jac'

fn findExtensions(line) :=
  let
    val extStr ≔ line ~* 1 /\{-#\s*LANGUAGE\s*(.*)#-\}/
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
curl -sL https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-weekly-breakthrough.csv -o /tmp/now-weekly-breakthrough.csv
ja ',[1.0-x%y] {ix>1}{`5:} {ix>1}{`11:}' -F, -i /tmp/now-weekly-breakthrough.csv
```

# Ctags

```
ja -F= '{%/let *[[:lower:]][[:alnum:]]*/}{(ix.`1)}' -i /development/dhall/dhall-kitty/conf.dhall
```

# Sum Total Population

```
wget https://burntsushi.net/stuff/worldcitiespop_mil.csv -O /tmp/worldcitiespop_mil.csv
ja -F, '(+)|0 {`5 ~ /\d+/}{`5:}' -i /tmp/worldcitiespop_mil.csv
```
