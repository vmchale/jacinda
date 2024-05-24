- inspiration: AWK, Haskell, J/APL/k, C (include)... (some architectural decisions mime GHC)
# Release
- [ ] Anchored streams
- [ ] Bug: fold1
  - [ ] two expressions with fold1 (and also gather-folds)
  - [x] fold: catmaybes, mapMaybes
  - [ ] fold-of-scan-of-mapMaybe...
# Documentation
- [ ] Document escaped characters
- [ ] tuples
- [ ] Document `%i`, `%s` in manpages
# Unicode
- [ ] `reintercalate` builtin
- [x] ∅ for monoidal identity
- [x] defeq ≔
  - [ ] ≜
- [x] arrows (types)
- [x] APL iota should be allowed instead of 'ix'
- [x] lambda
- [ ] ♯ in front of type variables
- [ ] 〈〈⟨⟩
- [x] unicode/apl for floor+ceiling
- [x] ⍬
# Features
- [ ] set variables on the command-line?
  - [ ] `fd` has `{}`, `{/}`, etc. so one could use it to pass in filenames
    while invoking...
- [ ] named columns (e.g. ``Ndx` for output of `readelf -Ws`)
- [ ] scan1
- [ ] "mod"
- [ ] `$>` operator like awk's `END`, allow stream to print and then present
      summary
- [ ] `Ord` for tuples, vectors
- [x] occurs check
- [ ] `drop` builtin/syntax?
- [x] `$(` `anchor`: evaluate multiple streams at once
- [x] `$1:` etc. parseable columns
- [ ] `:?` parsemaybe
- [ ] 0 literal could be a float
- [ ] `fail : a` builtin (unicode bottom?)
- [x] Error when type is ambiguous (e.g. `3: - 2:` or w/e)
- [x] mapMaybe, catMaybes? for `Witherable`...
  - [ ] vectors/lists should be a member!
- [ ] iota for vectors etc.
- [x] SCANS
  - [x] backend
- [ ] ignore case?
- [ ] environment variables
- [x] `x` need only be a keyword inside anonymous functions (dfns?)... otherwise it
      can be a variable!
  - [ ] that introduced a bug w/ nested dfns (euh)
- [x] `.1` etc. to extract tuples (arrays?)
  - [ ] tuples fr
- [ ] alex-style regex combinations, `$digit+` and `@string_in` or w/e
- [ ] ~~parse dates~~
- [½] map, filter, scan, prior for vectors &c.?
- [ ] lint for duplicate names at top-level
- [ ] list comprehensions or w/e
- [x] `\`$` as "all fields, as list" or something
- [x] `dedupBy` builtin
- [ ] Infix operators
  - [ ] `$>` (ice cream cone?)
  - [ ] `~?` (maybe match)
- [x] fold without seed
  - [x] `seq 10 | ja "[x+','+y]|'' \$0"`
  - [ ] foldMaybe
- [ ] helios distribution? https://github.com/oxidecomputer/helios
- [ ] ~~csv format: `FPAT = "([^,]*)|(\"[^\"]+\")"`~~
- [ ] substitute multiple to process
  ```
  vectorzmstreamzm0zi1zi0zi1zmb52aad89fd7b09efa5330a0a5ffa6e096514cefa8231d7a1e742d7f529db8237_DataziStreamziMonadic_zzipWith6_slow
  ```
  https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/symbol-names#variable-characters
- [ ] https://www.thegeekstuff.com/2010/01/8-powerful-awk-built-in-variables-fs-ofs-rs-ors-nr-nf-filename-fnr/
  - [ ] OFS
  - [ ] ORS
## Instances
- [ ] `Ord` instance for option?
## Syntax
- [ ] `$$` synonym for newline?
- [ ] `.[ ... ]` dfn where you specify that it's binary?
- [x] `?` for if... then?
- [ ] lineLength := (#")
- [ ] `{\<pat>}. $0` ... filter on stream expressions
  - [x] or #: maybe?
- [ ] `0$` or something... line, split by field separator,
# Examples
- [ ] https://github.com/wernsey/d.awk
- [ ] https://www.well.ox.ac.uk/~johnb/comp/awk/awk.html
- [ ] https://muhammadraza.me/2022/data-oneliners/
- [ ] http://cowlark.com/mercat/
- [x] mve.awk
- [ ] Awk scripts for building glibc
  - [ ] mach syscalls (print+summarize)
# Bugs
```
vanessa@Vanessas-Air jacinda % seq 100001 | cabal run ja -- run test/examples/evenOdd.jac
(50000 . 50001)
vanessa@Vanessas-Air jacinda % seq 1000001 | cabal run ja -- run test/examples/evenOdd.jac
(500002 . 500000)
```
- [ ]
```
echo $PATH | ja -F: "[x ~ /usr/] #. {|[x+'\n'+y]|>\`$}"
```
wrong result??
- [ ] Scoping pass so that ix outside of `{|...}` would be caught
- [ ] `ix` (line number) on filtered streams?
# Performance
- [ ] print redexes idk
- [ ] don't bother with `imap` if `ix` isn't used?
- [ ] https://github.com/ezrosent/frawk/blob/master/info/performance.md#test-data
- [ ] https://github.com/petewarden/dstkdata
- [ ] http://awka.sourceforge.net/compare.html
- [ ] bind let-stuff ... allow until later?
- [x] Rewrite rules: `eNorm` twice, rename twice, &c.
- [x] 32m to normalize expression (eClosed) on chess example
  - [x] space leak ay (foldWithCtx ... scanl' works) (manual space leak)
- [ ] not a fan of prettyprint
# Examples
- [ ] "context" builtin, like scan...
  ```
  otool -l $(locate libpng.dylib) | ja '{`1 ~ /^name/}{`2}'
  ```
  with `LD_ID_DYLIB` vs. `LD_LOAD_DYLIB` as "context"
  output of `objdump -D` after `Disassembly of section` (bookend)
  ```
  nm --defined-only /usr/local/lib/libgmp.a
  ```
- [ ] https://github.com/tenox7/ttyplot#examples
- [ ] https://github.com/arnoldrobbins/dformat/blob/master/dformat.awk
- [ ] https://awk.dev/
- [ ] https://github.com/gunnarmorling/1brc
  - [x] min/max
- [ ] https://www.stats.govt.nz/large-datasets/csv-files-for-download/
- [ ] https://catalog.data.gov/dataset/?res_format=CSV
- [ ] checksum?
- [½] `basename`, `pathchk` (lol)
- [ ] https://github.com/curl/trurl
- [ ] https://github.com/pharmbio/ptp-project/blob/master/exp/20180426-wo-drugbank/wo_drugbank_wf.go
- [ ] https://github.com/epogrebnyak/justpath
- [ ] https://github.com/socialfoundations/folktables
- [ ] https://catonmat.net/blog/wp-content/uploads/2008/09/awk1line.txt
- [ ] http://www.awklang.org/action/?nvgexp&child
- [ ] https://stackoverflow.com/q/2957684
- [ ] JQ_COLORS="0;90:0;37:0;37:0;37:0;32:1;37:1;37:1;34"
- [ ] https://github.com/rcoh/angle-grinder?tab=readme-ov-file#query-syntax
  - [ ] https://www.brandur.org/logfmt
## sed
- [ ] https://sed.sourceforge.io/grabbag/
- [ ] https://sed.sourceforge.io/
- [ ] seq 6 | sed -n 'N;l;D'
- [ ] https://gist.github.com/rubyroobs/77cc19f8985a12ca81e7d53789c144c5
- [ ] xz --robot --list *.xz | awk '/^totals/{print $5-$4}'
- [ ] https://gynvael.coldwind.pl/?lang=en&id=782
