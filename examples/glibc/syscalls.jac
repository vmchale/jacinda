fn printLines(lines) :=
  [x+'\n'+y]|'' lines;

let
  val calls := [x+' '+y]|'' $1
  val prologue := printLines {| sprintf 'sysno-%s = %s\nnargs-%s = %s' (`1 . `2 . `1 . `3)}
in sprintf '%s\nmach-syscalls := %s' (prologue . calls) end
