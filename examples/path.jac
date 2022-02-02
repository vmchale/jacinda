{. echo $PATH | ja run examples/path.jac
fn path(x) :=
  ([x+'\n'+y]) |> (splitc x ':');

path"$0
