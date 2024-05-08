let
  val calls := [x+' '+y]|'' $1
in 
  {| sprintf 'sysno-%s = %s\nnargs-%s = %s' (`1 . `2 . `1 . `3)} 
    $> sprintf 'mach-syscalls := %s' calls 
end
