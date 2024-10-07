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

Download a video using [yt-dlp](https://pypi.org/project/yt-dlp/), trimming
tracking information from the URL:

```
yt-dlp "$(pbpaste | ja '.?{|`1 ~* 1 /([^\?]*)/}')"
```

# Display Name of Terminal

Modification of a [StackExchange](https://askubuntu.com/a/476663) one-liner
using Awk.

```
ps -aux | rg "$(ps -p $$ -o ppid=)" | ja '{ix=1}{`*}'
```

# Filter GHC error messages to only those originating in some file

```
cabal -v0 build |& ja -R'\n\n' -F':' "{\`1='src/Ty.hs'}{\`0}"
```

# Find all distinct language extensions used in a project

```
fd '\.hs$' . -x ja '.?{|`0 ~* 1 /LANGUAGE\s*([a-zA-Z]*)\s*#-/}' -i | ja '~.$0'
```

This is not entirely rigorous; it does not account for `{-# LANGUAGE Xxx,Yyy #-}`.

This could be used to populate the `other-extensions` field in a `.cabal` file.

# NYC Vaccine Effectiveness

```
curl -sL https://raw.githubusercontent.com/nychealth/coronavirus-data/master/latest/now-weekly-breakthrough.csv | \
    ja ',[1.0-x%y] {ix>1}{`5:} {ix>1}{`17:}' -F,
```

# Ctags

```
ja -F= '{%/let\s*[[:lower:]][[:alnum:]]*/}{(⍳.`1)}' -i lib.dhall
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

# Extract All Symbols

```
nm -D $(which pandoc) \
    | sed 's/\([^z]\)zi/\1./g ;s/\([^z]\)zm/\1-/g; s/\([^z]\)zd/\1$/g; s/ZC/:/g; s/zz/z/g' \
    | ja '~..?{`2 ~ /^(T|t)$/}{`3 ~* 1 /([A-Za-z][A-Za-z0-9\-]*\-\d+(\.\d+)*)\-[0-9a-f]{4}/}'
```

## Extract Library Versions (Unstripped)

```
@include'lib/string.jac'

unlines¨{% /-lHS/}{captures `0 1 /-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)/}
```

```
readelf -p '.debug-ghc-link-info' $(which pandoc) | ja -R, '.?{|`0 ~* 1 /-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)/}'
```

```
readelf -p '.debug-ghc-link-info' $(which pandoc) | tr ',' '\n' | rg '\-lHS([A-Aa-z][A-Za-z0-9\-]*\d+(\.\d+)*)' -o -r'$1'
```

# Show Dynamic Library Dependencies

```
readelf -d $(which vim) | ja '.?{%/Shared library/}{`5 ~* 1 /\[(.*)\]/}'
```

# Show Dynamic Library Dependencies (Mac)

```
otool -l $(locate libpng.dylib | tail -n1) | ja -R'Load command' '{%/LC_LOAD_DYLIB/}{`7}'
```

## Show rpaths (Mac)

```
otool -l $(locate libapple.dylib | tail -n1) | ja -R 'Load command' '{%/LC_RPATH/}{`7}'
```

# Present RUNPATH (ELF)

```
readelf -d libapple.so | \
    ja '.?{%/RUNPATH/}{`5 ~* 1 /\[(.*)\]/}' | \
    tr ':' '\n'
```

# Show Machine Architecture (ELF)

```
readelf -h $(locate libpng.so | tail -n1) | ja -F':\s*' '{%/Machine/}{`2}'
```

# Tag Releases

Use the following to create a git tag by extracting the current version number
from the `.cabal` file:

```
git tag "$(ja -F'\s*:\s*' '{%/^\s*version/}{`2}' -i jacinda.cabal)"
```

# Imitate killall

To kill all running GHC processes:

```
kill $(ps u | ja '{%/ghc/}{`2}')
```

# Format All Code In a Haskell Project

```
fd '\.hs$' $(ja -F'\s*:\s*' '{%/hs-source-dirs/}{`2}' -i jacinda.cabal) -x stylish-haskell -i
```

# Count lines of code

```
fd '\.(c|h)$' -x wc -l | ja '(+)|0 $1:'
```

# Linecount

Count nonblank lines:

```
(+)|0 {%/./}{1}
```

Count blank lines:

```
(+)|0 {%/^$/}{1}
```

# Count Occurrences

```
(+)|0 {%/Bloom/}{1}
```

# Longest Line

```
max|_1 #¨$0
```

All lines >110 bytes:

```
{#`0>110}{`0}'
```

```
[#x>110] #. $0
```

Find all Haskell code >80 bytes wide

```
fd '\.hs$' -x ja '[#x>80] #. $0' -i
```

Is there a line >110 bytes?

```
(||)|#f {#`0>110}{#t}
```

```
(max|_1 #¨$0) > 110
```

# Prelude

One could write

```
sum := [(+)|0 x]

product := [(*)|1 x]

or := [(||)|#f x]

and := [(&)|#t x]

count := [(+)|0 [:1"x]

maximum := [max|_1 x]
```

# Print Line Number

```
(+)^0 [:1"$0
```

```
{|⍳}
```

# Generate Vim Tag Files for Futhark

```
futhark defs lib/github.com/vmchale/img-fut/img.fut | \
    ja -F'[\s+:]' "{|sprintf '%s\t%s\tcall cursor(%s,%s)' (\`2.\`3.\`4.(splitc \`5 '-').1)}"
```

# Extract C Dependencies

```
@include 'prelude/fn.jac'

fn dirname(x) :=
  x ~* 1 /([^\/]*\/)*(.*)/;

fn src(d) :=
  d ~* 1 /^\s*#include\s*"([^"]*)"/;

let
  val dir := fromMaybe '.' (dirname fp) + '/'
in [(dir +)"src x]:?$0 end
```

One would invoke like so (zsh):

```zsh
for d in $(ja run cdeps.jac -i src/frame.c); do realpath $d; done
```

This could be used for better dependency tracking in a Makefile, viz.

```makefile
C_SRC := $(shell for d in $$(ja run cdeps.jac -i src/main.c); do realpath $$d; done | ja "[x+' '+y]|> \$$0")

src/main.c: $(C_SRC)
```
