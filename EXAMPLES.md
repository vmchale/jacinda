# cdc

```
curl -sL https://data.cdc.gov/api/views/9mfq-cb36/rows.csv | sort | csvformat -D '|' | ja ':set fs:=/\|/; ,[(x.y)] ((-)\. {%/TX/}{`3:i}) {%/TX/}{`1}'
```

<!-- FIXME: succ diff?? -->

# NYC Vaccine Effectiveness


```
curl -sL https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-weekly-breakthrough.csv -o /tmp/now-weekly-breakthrough.csv
ja ',[1.0-x%y] {ix>1}{`5:f} {ix>1}{`11:f}' -F, -i /tmp/now-weekly-breakthrough.csv
```

# Ctags

```
ja -F= '{%/let *[[:lower:]][[:alnum:]]*/}{(ix.`1)}' -i /development/dhall/dhall-kitty/conf.dhall
```
