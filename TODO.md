- inspiration: AWK, Haskell, J/APL/k, C (include), ATS... (some architectural decisions mime GHC)
- [ ] xattr -d com.apple.quarantine
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
- [ ] ∅ for monoidal identity
- [x] defeq ≔
  - [ ] ≜
- [x] arrows (types)
- [x] APL iota should be allowed instead of 'ix'
- [x] lambda
- [ ] ♯ in front of type variables
- [ ] 〈〈⟨⟩
- [ ] unicode/apl for floor+ceiling
- [x] ⍬
# Features
- [ ] "mod"
- [ ] `$>` operator like awk's `END`, allow stream to print and then present
  summary
- [ ] `Ord` for tuples, vectors
- [x] occurs check
- [ ] `drop` builtin/syntax?
- [x] `$(` `anchor`: evaluate multiple streams at once
- [ ] `$1:` etc. parseable columns
- [ ] 0 literal could be a float
- [ ] `fail : a` builtin (unicode bottom?)
- [ ] Error when type is ambiguous (e.g. `3: - 2:` or w/e)
- [ ] `fail : a` builtin (unicode bottom?)
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
- [ ] parse dates
- [½] map, filter, scan, prior for vectors &c.?
- [ ] lint for duplicate names at top-level
- [ ] list comprehensions or w/e
- [ ] Expose captures
- [ ] `\`$` as "all fields, as list" or something
- [ ] `dedupBy` builtin
- [ ] Infix operators
  - [ ] `$>` (ice cream cone?)
  - [ ] `~?` (maybe match)
- [x] fold without seed
  - [x] `seq 10 | ja "[x+','+y]|'' \$0"`
  - [ ] foldMaybe
- [ ] Builtins
- [ ] helios distribution? https://github.com/oxidecomputer/helios
## Syntax
- [ ] `.[ ... ]` dfn where you specify that it's binary? lol
- [ ] `?` for if... then? lol
- [ ] lineLength := (#")
- [ ] `{\<pat>}. $0` ... filter on stream expressions
  - [x] or #: maybe?
- [ ] `0$` or something... line, split by field separator,
# Examples
- [ ] https://github.com/wernsey/d.awk
- [ ] https://www.well.ox.ac.uk/~johnb/comp/awk/awk.html
- [ ] https://muhammadraza.me/2022/data-oneliners/
- [ ] http://cowlark.com/mercat/
- [ ] mve.awk
- [ ] Awk scripts for building glibc
  - [ ] mach syscalls (print+summarize)
# Bugs
- [x] `echo $PATH | ja run examples/path.jac` (proper rename...)
- [ ] Scoping pass so that ix outside of `{|...}` would be caught
# Performance
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
- [ ] https://github.com/tenox7/ttyplot#examples
- [ ] https://github.com/arnoldrobbins/dformat/blob/master/dformat.awk
- [ ] https://awk.dev/
- [ ] https://github.com/gunnarmorling/1brc
  - [ ] min/max
- [ ] https://www.stats.govt.nz/large-datasets/csv-files-for-download/
- [ ] https://catalog.data.gov/dataset/?res_format=CSV
- [ ] checksum?
- [ ] `basename`, `pathchk` (lol)
- [ ] https://github.com/curl/trurl
