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
- [ ] lambda
- [ ] ♯ in front of type variables
# Features
- [x] SCANS
  - [x] backend
- [ ] dyadic infix/prior? Something for successive differences!
- [x] string concatenation
- [x] `x` need only be a keyword inside anonymous functions (dfns?)... otherwise it
  can be a variable!
  - [ ] that introduced a bug w/ nested dfns (euh)
- [ ] better error messages ja: ./Data/Vector/Generic.hs:257 ((!)): index out of bounds (5,2)
- [ ] `NR` - number of records (per line) also `-1 (last)
- [ ] printf
  - [ ] array/vector type (integer indexed)
- [x] `[:` as shorthand for `const`
- [ ] `.1` etc. to extract tuples (arrays?)
- [ ] alex-style regex combinations, `$digit+` and `@string_in` or w/e
- [ ] parse dates
## Syntax
- [ ] `;` vs. `val`?
- [ ] `.[ ... ]` dfn where you specify that it's binary? lol
- [ ] `?` for if... then? lol
- [ ] lineLength := (#")
- [x] `\\` (backslash) for "prior", succ diffs
- [ ] `{\<pat>}. $0` ... filter on stream expressions
  - [ ] or #: maybe?
# Examples
- [ ] https://www.well.ox.ac.uk/~johnb/comp/awk/awk.html
- [ ] http://cowlark.com/mercat/
- [ ] get `(max|_1 #"$0) > 72` to compile
