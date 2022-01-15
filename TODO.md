- inspiration: AWK, Haskell, J/APL, k, Dhall (expression imports), C (include), ATS... (some architectural decisions mime GHC)
- [ ] regex errors should include position of regex
- [x] separate syntax for filtering a stream, e.g. {/pat/}[$0] or w/e
  - [x] still have {/pat/}{field}
- [ ] REPL would be good.
- [ ] xattr -d com.apple.quarantine
# Documentation
- [ ] Document escaped characters
- [ ] tuples
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
# Features
- [ ] `$0:` etc. parseable columns
- [ ] 0 literal could be a float
- [ ] `fail : a` builtin (unicode bottom?)
- [x] Operator syntax for floor/ceiling?
  - [x] `|.` for instance? `|'`?
- [ ] Witherable typeclass, vectors/lists should be a member!
- [ ] Error when type is ambiguous (e.g. `3: - 2:` or w/e)
- [ ] `fail : a` builtin (unicode bottom?)
- [x] Operator syntax for floor/ceiling?
- [ ] Witherable typeclass, vectors/lists should be a member!
- [ ] mapMaybe, catMaybes? for `Witherable`...
  - [ ] `:?` or `.?` as `mapMaybe` and `catMaybes`??
  - [ ] vectors/lists should be a member!
- [x] `match`
- [x] `splitc` - split on char! (easier/faster)
- [ ] iota for vectors etc.
- [x] SCANS
  - [x] backend
- [x] dyadic infix/prior? Something for successive differences!
- [x] string concatenation
- [x] `x` need only be a keyword inside anonymous functions (dfns?)... otherwise it
  can be a variable!
  - [ ] that introduced a bug w/ nested dfns (euh)
- [x] better error messages ja: ./Data/Vector/Generic.hs:257 ((!)): index out of bounds (5,2)
- [ ] `NR` - number of records (per line) also `-1 (last)
- [x] printf
  - [x] array/vector type (integer indexed)
- [x] `[:` as shorthand for `const`
- [x] `.1` etc. to extract tuples (arrays?)
  - [ ] tuples fr
- [ ] alex-style regex combinations, `$digit+` and `@string_in` or w/e
- [ ] parse dates
- [ ] `Parseable` class - implement
- [½] map, filter, scan, prior for vectors &c.?
- [ ] lint for duplicate names at top-level
- [ ] list comprehensions or w/e
- [ ] `Option` is not a functor
## Syntax
- [x] `;` vs. `val`?
- [ ] `.[ ... ]` dfn where you specify that it's binary? lol
- [ ] `?` for if... then? lol
- [ ] lineLength := (#")
- [x] `\\` (backslash) for "prior", succ diffs
- [ ] `{\<pat>}. $0` ... filter on stream expressions
  - [x] or #: maybe?
# Examples
- [ ] https://www.well.ox.ac.uk/~johnb/comp/awk/awk.html
- [ ] http://cowlark.com/mercat/
- [ ] mve.awk
# Bugs
- [ ] '[(split y /-/).1]"$0' -> "bare reserved variable" whoops
- [ ] e "[x+' '+y]|' ' split '01-23-1987' /-/"
# Performance
- [x] Rewrite rules: `eNorm` twice, rename twice, &c.
- [x] 32m to normalize expression (eClosed) on chess example
  - [x] space leak ay (foldWithCtx ... scanl' works) (manual space leak)
