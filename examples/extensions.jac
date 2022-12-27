{. shows extensions used + total line count
{. invoke like so:
{. fd '\.hs$' -x ja run extensions.jac -i
fn count(x):=
  (+)|0 [:1"x;

let
  val ext := count {%/\{-#\s*LANGUAGE\s*(.*)#-\}/}{`0}
  val tot := count $0
in sprintf '%s\t%i\t%i' (fp.ext.tot) end
