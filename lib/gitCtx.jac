@include'prelude/fn.jac'
@include'lib/maybe.jac'

fn mMatch(p, str) :=
  if p str
    then Some str
    else None;

fn step(p, ctx, line) :=
  let
    val fpCtx ≔ line ~* 1 /diff --git\s+([^\s]+)/
    val mLine ≔ mMatch p line
  in (alternative (ctx->1) fpCtx.mLine) end;

fn process(x) :=
  let
    val fpCtx ≔ fromMaybe 'WARN' (x->1)
    val line ≔ (λl. sprintf'%s: %s' (fpCtx.l))"(x->2)
  in line end;

fn main(p) :=
  process:?(step p)^(None.None)$0;
