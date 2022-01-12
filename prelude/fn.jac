fn sum(x) :=
  (+)|0 x;

fn drop(n, str) :=
  let val l := #str
    in substr str n l end;

fn take(n, str) :=
  substr str 0 n;

{. rounds down on .5
fn round(x) := floor (x+0.5);

fn itostring() :=
  sprintf '%i';

fn any(p, xs) :=
  (||)|#f p"xs;

fn all(p, xs) :=
  (&)|#t p"xs;

fn id(x) := x;

fn fromMaybe(a, x) :=
  option a [x] x;

fn replace(re, t, str) :=
  [x+t+y]|'' (split str re);
