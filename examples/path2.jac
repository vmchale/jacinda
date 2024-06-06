{. echo $PATH | ja run examples/path.jac

fn path(x) :=
  reintercalate '\n' (splitc x ':');

path¨$0
