{. https://www.gnu.org/software/sed/manual/sed.html#Joining-lines

fn go(acc, line) :=
  let 
    val next := option [x] [(x+)] (acc->1)
  in
    option (None . (Some (next line))) [(Some (next x) . None)] (line ~* 1 /([^\\]*)\\/)
  end;

[x->2]:?(go^(None . None) $0)
