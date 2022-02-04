fn step(acc, this) :=
  if this = acc->1
    then (this . None)
    else (this . Some this);

(->2):?step^(''.None) $0
