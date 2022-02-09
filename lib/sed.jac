@include'prelude/fn.jac'

{. example: (replace /^var/ 'export const')"$0
fn replace1(re, str, line) :=
  let 
    val insert := \line. \str. \ixes.
      take (ixes->1) line + str + drop (ixes->2) line
  in option line (insert line str) (match line re) end;
