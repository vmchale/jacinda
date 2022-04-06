{. fd -t f -x wc -l | ja run cloc.jac

@include'prelude/fn.jac'

fn count(xs) :=
  (+)|0 [:1"xs;

{. TODO: clojure
let
  val bqn := {%/\.bqn$/}{`1:i}
  val hs := {%/\.hs$/}{`1:i}
  val ja := {%/\.jac$/}{`1:i}
  val pl := {%/\.pl$/}{`1:i}
  val sed := {%/\.sed$/}{`1:i}
  val asm := {%/\.(S|asm)$/}{`1:i}
  val rb := {%/\.rb$/}{`1:i}
  val py := {%/\.py$/}{`1:i}
  val c := {%/\.c$/}{`1:i}
  val awk := {%/\.awk$/}{`1:i}
  val m4 := {%/\.m4$/}{`1:i}
  val make := {%/((M|GNUm|m)akefile|\.mk)$/}{`1:i}
  val batch := {%/\.bat$/}{`1:i}
in sprintf'Haskell:\t%i\t%i\nBQN:\t\t%i\t%i\nJacinda:\t%i\t%i\nC:\t\t%i\t%i\nPython:\t\t%i\t%i\nPerl:\t\t%i\t%i\nSed:\t\t%i\t%i\nAssembly:\t%i\t%i\nRuby:\t\t%i\t%i\nAwk:\t\t%i\t%i\nM4:\t\t%i\t%i\nMakefile:\t%i\t%i\nBatch:\t\t%i\t%i'
  ( count hs . sum hs
  . count bqn . sum bqn
  . count ja . sum ja
  . count c . sum c
  . count py . sum py
  . count pl . sum pl
  . count sed . sum sed
  . count asm . sum asm
  . count rb . sum rb
  . count awk . sum awk
  . count m4 . sum m4
  . count make . sum make
  . count batch . sum batch
  )
end
