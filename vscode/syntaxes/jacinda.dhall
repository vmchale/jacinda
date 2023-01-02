-- https://macromates.com/manual/en/language_grammars
let TextMate =
      < MatchRe : { match : Text, name : Text }
      | BeginEnd : { begin : Text, end : Text, name : Text }
      >

let mkTextMate =
      \(tm : TextMate) ->
        merge
          { MatchRe =
              \(x : { match : Text, name : Text }) ->
                { match = Some x.match
                , name = x.name
                , begin = None Text
                , end = None Text
                }
          , BeginEnd =
              \(x : { begin : Text, end : Text, name : Text }) ->
                { match = None Text
                , begin = Some x.begin
                , end = Some x.end
                , name = x.name
                }
          }
          tm

in  { fileTypes = [ "jac" ]
    , name = "jacinda"
    , scopeName = "source.jacinda"
    , patterns =
      [ mkTextMate
          (TextMate.MatchRe { match = "{\\..*\$", name = "comment.line" })
      , mkTextMate
          (TextMate.MatchRe { match = "#(t|f)", name = "constant.language" })
      , mkTextMate
          ( TextMate.MatchRe
              { match =
                  "(:set|:flush|@include|(let|in|val|end|fn|if|then|else)[^a-zA-Z])"
              , name = "keyword"
              }
          )
      , mkTextMate
          ( TextMate.MatchRe
              { match = "[a-z][a-zA-Z]*", name = "entity.name.function" }
          )
      , mkTextMate
          ( TextMate.BeginEnd
              { begin = "/", end = "[^\\\\]/", name = "string.regexp" }
          )
      , mkTextMate
          (TextMate.BeginEnd { begin = "'", end = "[^\\\\]'", name = "string" })
      ]
    }
