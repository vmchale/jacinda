fn colmap(str) :=
  let val p := split str /.*\^/ in p end;

let
  val col := {%/^[\t-]*\^/}{`0}
in col end
