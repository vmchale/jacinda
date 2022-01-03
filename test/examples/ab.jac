let
  val asec := {`2 ~ /^[aA][0-9]+/}{1}
  val bsec := {`2 ~ /^[bB][0-9]+/}{1}
  val sum := [(+)|0 x]
in (sum asec . sum bsec) end
