fn processLine(b) :=
  let 
    val pre := (split b /-/).1
    val l := #pre
    val l0 := l-1
    val res := substr pre (l-1) l
  in res:i end;

let
  val sum := [(+)|0 x]
  val white := sum {`1~/\d/ & `1:i=1}{1}
  val black := sum {`1~/\d/ & `1:i=0}{1}
  val draw := sum {`1~/\d/ & `1:i=2}{1}
  val total := white + black + draw
in (total . white . black . draw) end
