-- https://macromates.com/manual/en/language_grammars
let map =
      https://prelude.dhall-lang.org/List/map
        sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let TextMate =
      < MatchRe : { match : Text, name : Text }
      | BeginEnd : { begin : Text, end : Text, name : Text }
      >

let mkTextMate =
      λ(tm : TextMate) →
        merge
          { MatchRe =
              λ(x : { match : Text, name : Text }) →
                { match = Some x.match
                , name = x.name
                , begin = None Text
                , end = None Text
                }
          , BeginEnd =
              λ(x : { begin : Text, end : Text, name : Text }) →
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
        map
          TextMate
          { match : Optional Text
          , begin : Optional Text
          , end : Optional Text
          , name : Text
          }
          mkTextMate
          [ TextMate.MatchRe { match = "{\\..*\$", name = "comment.line" }
          , TextMate.MatchRe { match = "#!.*\$", name = "comment.line" }
          , TextMate.MatchRe { match = "#(t|f)", name = "constant.language" }
          , TextMate.MatchRe
              { match =
                  "(:set|:flush|@include|(let|in|val|end|fn|if|then|else)[^a-zA-Z])"
              , name = "keyword"
              }
          , TextMate.MatchRe
              { match =
                  "(splitc|sprintf|option|match|captures|fp|nf|rs|fs|ors|ofs|ix|substr|split|min|max|Some|None|mapMaybe|dedup|filter|fold|fold1|scan|dedupOn|catMaybes)"
              , name = "constant.language"
              }
          , TextMate.MatchRe
              { match = "[a-z][a-zA-Z]*", name = "entity.name.function" }
          , TextMate.BeginEnd
              { begin = "/", end = "[^\\\\]/", name = "string.regexp" }
          , TextMate.BeginEnd { begin = "'", end = "[^\\\\]'", name = "string" }
          ]
    }
