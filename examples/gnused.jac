{. https://www.gnu.org/software/sed/manual/sed.html#Joining-lines

fn go(acc, line) :=
  let 
    val accStr := option '' [x] (acc->1)
  in
    option (None . Some (accStr+line)) [(Some (accStr+x) . None)] (line ~* 1 /([^\\]*)\\/)
  end;

[x->2]:?(go^(None.None) $0)
