{. see test/examples/data/countries, awk book
let
  val popBy := [(+)|0 {% x}{`3:i}]
in (popBy /Asia/ . popBy /Europe/) end
