{. s/^var/export const/ in sed

@include'prelude/fn.jac'

fn insert(line, str, ixes) :=
  take (ixes->1) line + str + drop (ixes->2) line;

fn replace(re, str, line) :=
  option line (insert line str) (match line re);

(replace /^var/ 'export const')"$0
