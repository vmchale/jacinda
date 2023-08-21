let
  val list := [x+', '+y]|>[x ~* 1 /\{-#\s*LANGUAGE\s*([^\s]*)\s*#-\}/]:?$0
in sprintf '{-# LANGUAGE %s -#}' list end
