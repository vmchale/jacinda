fn step(acc, this) :=
  ?this = acc->1;
    (this . None);
    (this . Some this);

(->2):?step^(''.None) $0
