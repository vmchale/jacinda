{. FIXME: can't handle \r lol
let
  val sum := [(+)|0 x]
  val words := sum {|nf}
  val lines := sum {|1}
  val bytes := sum {|#`0+1}
in sprintf'%i %i %i' (lines.words.bytes) end
