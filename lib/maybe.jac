fn isJust(x) :=
  option #f ([:#t) x;

fn isNothing(x) :=
  option #t ([:#f) x;

{. Haskell's (<|>)
fn alternative(a, b) :=
  option a Some b;
