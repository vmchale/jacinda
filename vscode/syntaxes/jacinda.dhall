-- https://macromates.com/manual/en/language_grammars
{ fileTypes = [ "jac" ]
, name = "jacinda"
, scopeName = "source.jacinda"
, patterns =
  [ { match = "{\\..*\$", name = "comment.line" }
  , { match = "#(t|f)", name = "constant.language" }
  , { match = "(:set|:flush|@include|let|in|val|end|fn|if|then|else)"
    , name = "keyword"
    }
  ]
}
