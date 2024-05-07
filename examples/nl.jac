@include'lib/string.jac'

fn step(acc, line) :=
  ?empty line;
    (acc->1 . '');
    (acc->1 + 1 . line);

fn process(x) :=
  ?!empty (x->2); sprintf '    %i\t%s' x; '';

process"step^(0 . '') $0
