fn intercalate(b, strs) :=
  [x+b+y]|'' strs;

fn replace(re, t, str) :=
  [x+t+y]|'' (split str re);
