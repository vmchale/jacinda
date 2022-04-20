{. program equiv to s/^(type|interface)/export \1/

@include'prelude/fn.jac'

fn replaceAtWith(line, ixes) :=
  let
    val pre := take (ixes->1) line
    val at := drop (ixes->1) line {. in our case
  in pre + 'export ' + at end;

fn replaceAt(line) :=
  option line (replaceAtWith line) (match line /^(type|interface)/);

replaceAt"$0
