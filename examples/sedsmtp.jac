{. https://www.gnu.org/software/sed/manual/sed.html#Joining-lines
{. SMTP example

fn go(acc, line) :=
  option (acc+'\n'+line) [acc+' '+x] (line ~* 1 /^\s+(\.*)/);

go|>$0
