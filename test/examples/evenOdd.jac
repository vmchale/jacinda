fn count(x) ≔
  (+)|0 [:1"x;

fn isEven() :=
  (~ /(0|2|4|6|8)$/);

fn isOdd() :=
  (~ /(1|3|5|7|9)$/);

let
  val iStream := $0
  val even := count (isEven #. iStream)
  val odd := count (isOdd #. iStream)
  val tens := (+)|> {%/0$/}{1}
  val total := odd + even
in (total . even . odd . tens) end
