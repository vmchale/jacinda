fn count(xs) := (+)|0 [:1"xs;

fn nfWin(l) :=
  option 0 (\l.count(split l /\s+/)) (l ~* 1 /(.*)\r/);

let
  val sum := [(+)|0 x]
  val words := sum {|nfWin`0}
  val lines := sum {|1}
  val bytes := sum {|#`0+1} {. for newline
in sprintf'%i %i %i' (lines.words.bytes) end
