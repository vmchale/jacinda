{. FIXME: figure out @include'lib/string.jac'??
@include'lib/string.jac'

fn step(acc, line) :=
  if empty line
    then (acc->1 . '')
    else (acc->1 + 1 . line);

fn process(x) :=
  if !empty (x->2)
    then sprintf '    %i\t%s' x
    else '';

process"step^(0 . '') $0
