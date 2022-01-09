- inspiration: AWK, Haskell, J/APL, k, Dhall (expression imports), C (include), ATS... (some architectural decisions mime GHC)
- [ ] regex errors should include position of regex
- [ ] separate syntax for filtering a stream, e.g. {/pat/}[$0] or w/e
  - [ ] still have {/pat/}{field}
- [ ] REPL would be good.
- [ ] xattr -d com.apple.quarantine
# Unicode
- [ ] defeq ≜ ≔
- [ ] arrows (types)
- [ ] APL iota should be allowed instead of 'ix'
- [x] lambda
- [ ] ♯ in front of type variables
- [ ] ⟨
# Features
- [ ] `match`
- [x] SCANS
  - [x] backend
- [x] dyadic infix/prior? Something for successive differences!
- [x] string concatenation
- [x] `x` need only be a keyword inside anonymous functions (dfns?)... otherwise it
  can be a variable!
  - [ ] that introduced a bug w/ nested dfns (euh)
- [ ] better error messages ja: ./Data/Vector/Generic.hs:257 ((!)): index out of bounds (5,2)
- [ ] `NR` - number of records (per line) also `-1 (last)
- [ ] printf
  - [x] array/vector type (integer indexed)
- [x] `[:` as shorthand for `const`
- [ ] `.1` etc. to extract tuples (arrays?)
  - [ ] tuples fr
- [ ] alex-style regex combinations, `$digit+` and `@string_in` or w/e
- [ ] parse dates
<<<<<<< HEAD
- [ ] map, filter, scan, prior for vectors &c.?
=======
- [ ] lint for duplicate names at top-level
>>>>>>> 234c9ae (doc)
## Syntax
- [x] `;` vs. `val`?
- [ ] `.[ ... ]` dfn where you specify that it's binary? lol
- [ ] Redo string sytnax (`'` for map, `"` for etc.)
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
