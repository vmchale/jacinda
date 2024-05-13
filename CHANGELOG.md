# 3.0.1.1

  * `fp` is defined (as `(stdin)`) when reading from stdin
  * Bug in row typing for fields where `->0` is treated as legitimate and fails
    with unclear error at runtime

# 3.0.1.0

  * Add `,,` (bookend)
  * Add `$>`; stream-and-report

# 3.0.0.0

  * Rewrite backend to always stream/not blow up memory
  * Better error messages when a field is out of bounds
  * Better error message on empty `|>` (`fold1`)
  * Fix parsing bug in curried binary operators
  * Add `--asv` and `--usv` command-line flags
  * Add `:set asv;` `:set usv;` to language
  * Add `Witherable` instance for `List`
  * Add `subs` builtin

# 2.0.3.0

  * Handle some parsing/currying cases (still hinky)
  * Add `sub1` builtin like AWK's `sub`
  * Better presentation of regex engine errors
  * Faster float printing
  * Rewrite pretty-printer to parenthesize correctly.

# 2.0.2.0

  * Add support for custom record separators
  * Faster float parsing
  * Add \`$ builtin
  * Allow escaped `/` within regex
  * Round out support for `$0:f` &c.

# 2.0.1.0

  * Add `mapMaybe`, `dedup`, `filter`, `fold`, `fold1`, `scan`, `dedupOn`,
    `catMaybes` synonyms.

# 2.0.0.0

  * Scrap `HasField` typeclass; add row types
  * Expressions with multiple folds no longer blow up memory (🤞)
  * Fix many bugs
  * Unicode syntax works from command-line

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
