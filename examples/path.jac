{. echo $PATH | ja run examples/path.jac
{. ja run examples/path.jac <(echo $PATH)
fn path(x) :=
  ([x+'\n'+y]) |> (splitc x ':');

path¨$0
