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

# Extract [Fixity Declarations for HLint](https://github.com/ndmitchell/hlint?tab=readme-ov-file#why-doesnt-hlint-know-the-fixity-for-my-custom--operator)

```
{%/infix(r|l)?\s+\d+/}{sprintf '- fixity: %s' `0}
```

Equivalently:

```
[sprintf '- fixity: %s' x]¨(~/infix(r|l)?\s+\d+/)#.$0
```

This is equivalent to `hlint --find src/FILE.hs | rg '^- fixity:'`, except it
works on Happy, Alex, etc. preprocessor files.

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

## Laconic Style

Suppose we have a Haskell file with several GHC pragmas and we wish to condense them to a single line.

```haskell
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
```

Then we could use

```
let
  val list := [x+', '+y]|>[x ~* 1 /\{-#\s*LANGUAGE\s*([^\s]*)\s*#-\}/]:?$0
in sprintf '{-# LANGUAGE %s -#}' list end
```

We'd get:

```haskell
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, OverloadedStrings, TypeFamilies -#}
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

# Inflation

```
curl https://www.stats.govt.nz/assets/Uploads/Food-price-index/Food-price-index-September-2023/Download-data/food-price-index-september-2023-weighted-average-prices.csv -o food-prices.csv
ja --csv '(%)\. {%/Apple/}{`3:}' -i food-prices.csv
```

# Get Environment Variable

```
printenv | ja -F= '{%/^PATH/}{`2}'
```
