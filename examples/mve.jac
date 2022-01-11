fn colmap(str) :=
  let val p := split str /.*\^/ in p end;

{. option 0 (->2) (match '---------^' /.*\^/)

let
  val col := {%/^[\t-]*\^/}{`0}
in col end
