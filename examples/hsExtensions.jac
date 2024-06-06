fn findExtensions(line) :=
  let
    val extStr ≔ line ~* 1 /\{-#\s*LANGUAGE\s*(.*)#-\}/
    val extList ≔ (\s.split s /,\s*/)"extStr
  in extList end;

~.(λx.(reintercalate'\n')¨(findExtensions x)):?$0
