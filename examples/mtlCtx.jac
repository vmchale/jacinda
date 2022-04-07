{. finds any line matching /MonadState/ or /MonadError/ without
{. an INLINABLE pragma on the preceding line

@include'lib/maybe.jac'

fn step(ctx, line) :=
  let
    val fpCtx ≔ line ~ /\{-#.*INLINABLE.*#-\}/
    val mLine ≔
      if (ctx->1) || line !~ /:.*Monad(State|Error)/
        then None
        else Some line
  in (fpCtx.mLine) end;

(->2):?step^(#f.None)$0
