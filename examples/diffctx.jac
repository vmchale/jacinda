{. git diff master | rg '(\+|-).*TODO'

@include'prelude/fn.jac'
@include'lib/maybe.jac'

fn step(needle, ctx, line) :=
  let 
    val fpCtx ≔ line ~* 1 /diff --git\s+([^\s]+)/
    val mLine ≔ line ~? needle
  in (alternative (ctx->1) fpCtx.mLine) end;

fn process(x) :=
  let
    val fpCtx ≔ fromMaybe 'WARN' (x->1)
    val line ≔ (λl. sprintf'%s: %s' (fpCtx.l))¨(x->2)
  in line end;

process:?(step/(\+|-).*TODO/)^(None.None)$0
