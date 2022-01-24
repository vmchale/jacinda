# Find all distinct language extensions used in a project

```
rg '\{-#.*LANGUAGE.*#-}' --no-filename | ja '~.$3'
```

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
