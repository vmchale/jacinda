# 1.2.0.0

  * `~`, `!~` builtins require that the regex be the second argument.

# 1.1.0.0

  * Add builtin for last field
  * Performance improvements
  * Fix bug in how fields were split
  * `:` works on column literals
  * Add `#*` (list length) builtin

# 1.0.0.0

  * Generalize type of `\.` (prior)
  * Reintroduce monomorphism restriction (oops)
  * Better errors for ambiguous types
  * Multiple folds no longer blow up memory in some cases
  * `option` can be curried (parse bug fixed)

# 0.3.1.0

  * Performance improvements
  * Bug fix, dfns are renamed properly
  * Add `-.` negate function
  * Work with shell shebangs
  * Implement `=` for boolean
  * Add `captures` as a builtin
  * Add `|>`, fold without seed
  * Allow `fn...` declarations with no arguments and no parentheses
  * Add conditionals
  * Fix bug in normalizing `Some` and `None`
  * Fix bug in indexing + filter
  * Fix bug in polymorphic functions used at multiple sites
  * Change parsing/rewrite so `f a b + c` parses as `(f a b) + c` rather than `f a (b + c)`
  * Fix bug in parser rewriting in `@include`d files
  * Include searches current directory

# 0.3.0.0

  * Fix renaming bug that was inveigling folds with lambdas
  * Add `nf` builtin
  * Add deduplication builtin (`~.`)
  * Add anchor ability to print multiple streams
  * Make `Option` a functor
  * Add `Witherable` class, `:?` (mapMaybe)
  * Allow file `@include`s (crude library capability)
  * Fix typos in manpage

# 0.2.1.0

  * Add `fp` builtin
  * Add `:` unary operator
  * Floor/ceiling operators
  * `Some` and `None` literals

# 0.2.0.0

  * Complete implementation of folds/maps for lists
  * Fix space leak in folds
  * Fix line splitting (no longer discard blank lines)
  * Dfn fix
  * Allow escaped characters in strings
  * Add several builtin functions
  * Location information when reporting errors related to typeclasses
  * Option type
  * Selectors for tuples

# 0.1.0.0

* Initial release
