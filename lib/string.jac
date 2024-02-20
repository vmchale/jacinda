fn intercalate(b) :=
  ([x+b+y] |>);

fn replace(re, t, str) :=
  [x+t+y] |> (split str re);

{. censor, bowlderize, expunge?
fn redact(re, x) :=
  sub1 re '' x;

fn empty(str) :=
  #str = 0;
