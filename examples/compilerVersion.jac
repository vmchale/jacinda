@include'lib/string.jac'

:set rs:=/String dump of section '[\.a-z\-]*':\n/;
:set fs:=/\s*\[[ a-f0-9]*\]\s*/;

fn tr(str) :=
  captures str 1 /([^\n]*)\n+/;

let
  val rr := /GHC|GCC|clang|mold|rustc|gold/
in unlines"{% rr}{tr (unlines ((~rr) #. `$))} end
