@include'lib/string.jac'

:set rs:=/String dump of section '[\.a-z\-]*':\n/;
:set fs:=/\s*\[[ a-f0-9]*\]\s*/;

unlines"{%/GCC|GHC|clang|mold|rustc|gold/}{captures (unlines ([x ~ /GCC|GHC|clang|mold|rustc|gold/] #. `$)) 1 /([^\n]*)\n+/}
