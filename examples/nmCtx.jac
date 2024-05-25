{. fd '\.a$' /usr -x nm --defined-only

@include'prelude/fn.jac'
@include'lib/maybe.jac'

fn step(ctx, line) :=
  let
    val fpCtx := line ~* 1 /(.*\.o):$/
    val mSym := line ~* 1 /^[0-0a-f]{16}.{3}(.*)/
  in (alternative (ctx->1) fpCtx.mSym) end;

fn process(x) :=
  let
    val fpCtx := fromMaybe 'WARN' (x->1)
    val line := (λl. sprintf'%s: %s' (fpCtx.l))¨(x->2)
  in line end;

process:?step^(None.None)$0
