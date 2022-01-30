{. echo $PATH | ja run examples/path.jac

@include'lib/string.jac'

fn path(x) :=
  intercalate '\n' (splitc x ':');

path"$0
