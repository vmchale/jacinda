fn sum(x) :=
  (+)|0 x;

fn drop(n, str) :=
  let val l := #str
    in substr str n l end;

fn take(n, str) :=
  substr str 0 n;
