{. readelf -p .comment -p .note.gnu.gold-version $(which ja) | ja run compilerVersion.jac
@include'lib/string.jac'

:set rs:=/String dump of section '[\.a-z\-]*':\n/;
:set fs:=/\s*\[[ a-f0-9]*\]\s*/;

unlines¨{% /[^\s]/}{(λstr. str ~* 1 /([^\n]*)\s*/):?([x ~ /GHC|GCC|clang|mold|rustc|gold/] #. `$)}
