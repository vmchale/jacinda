{. cat *.pgn | grep "Result" | awk ...
{. awk '{ split($0, a, "-"); res = substr(a[1], length(a[1]), 1); if (res == 1) white++; if (res == 0) black++; if (res == 2) draw++;} END { print white+black+draw, white, black, draw }'
{. cat *.pgn | grep "Result" | awk '{ split($0, a, "-"); res = substr(a[1], length(a[1]), 1); print $0, a[1], res;}'

{. N.B. should be easier? count records/columns
{. awk '{games += $1; white += $2; black += $3; draw += $4; } END { print games, white, black, draw }'

let
  val l := [(+)|0 x]
  val white := l {`1:i = 1}{1}
  val black := l {`1:i = 0}{1}
  val draw := l {`1:i = 2}{1}
in (white + black + draw . white . black . draw) end
