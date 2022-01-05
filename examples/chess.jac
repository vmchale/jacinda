let
  val sum := [(+)|0 x]
  val white := sum {`1~/\d/ & `1:i=1}{1}
  val black := sum {`1~/\d/ & `1:i=0}{1}
  val draw := sum {`1~/\d/ & `1:i=2}{1}
in white end {. (white . black . draw) end
