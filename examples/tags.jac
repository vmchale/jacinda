fn mkEx(s) :=
  '/^' + s + '$/;';

fn processStr(l) :=
  let
    val s := l->1
    val line := split s /[ \(:]+/
    val col := #line.1 + 1
    val outLine := sprintf '%s\t%s\tcall cursor(%i,%i)' (line.2 . fp . l->2 . col)
  in outLine end;

processStr¨{%/fn +[[:lower:]][[:latin:]]*.*:=/}{(`0 . ix)}
