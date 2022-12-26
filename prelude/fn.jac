fn sum(x) :=
  (+)|0 x;

fn drop(n, str) :=
  let val l := #str
    in substr str n l end;

fn take(n, str) :=
  substr str 0 n;

{. rounds down on .5
fn round(x) := |. (x+0.5);

fn itostring :=
  sprintf '%i';

fn any(p, xs) :=
  (||)|#f p"xs;

fn all(p, xs) :=
  (&)|#t p"xs;

fn id(x) := x;

fn fromMaybe(a, x) :=
  option a [x] x;

fn last :=
  ([y]|>);

fn head :=
  ([[:x]|>);

{. fold two on the same stream
fn foldTwo(op0, op1, seed0, seed1, stream) :=
  let
    val go := \acc. \line. (op0 (acc->1) line . op1 (acc->2) line)
  in go|(seed0.seed1) stream end;
