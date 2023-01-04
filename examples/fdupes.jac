{. invoke like so:
{. fd . -t f -x md5sum | sort | ja run fdupes.jac
{. inspired by Yann Le Du: https://twitter.com/Yann_Le_Du/status/1610299070819729410

fn step(acc, this) :=
  if (substr this 0 32) = (substr (acc->1) 0 32)
    then (this . Some (this + '\n' + acc->1))
    else (this . None);

(->2):?step^(''.None) $0
