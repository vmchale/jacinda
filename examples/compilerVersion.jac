{. readelf -p .comment -p .note.gnu.gold-version $(which ja) | ja run compilerVersion.jac
@include'lib/string.jac'

:set rs:=/String dump of section '[\.a-z\-]*':\n/;
:set fs:=/\s*\[[ a-f0-9]*\]\s*/;

fn tr(str) :=
  captures str 1 /([^\n]*)\n+/;

unlines¨{% /[^\s]/}{tr (unlines ([x ~ /GHC|GCC|clang|mold|rustc|gold/] #. `$))}
