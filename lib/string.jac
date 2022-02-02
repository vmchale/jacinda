fn intercalate(b) :=
  ([x+b+y] |>);

fn replace(re, t, str) :=
  [x+t+y] |> (split str re);

fn empty(str) :=
  #str = 0;
