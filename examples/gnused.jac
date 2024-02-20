{. https://www.gnu.org/software/sed/manual/sed.html#Joining-lines
@include'prelude/fn.jac'

fn go(acc, line) :=
  option (acc+'\n'+line) [x+line] (acc ~* 1 /([^\\]*)\\/);

go|>$0
