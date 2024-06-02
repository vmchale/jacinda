{
    module Csv (p) where
    
import L
import qualified Data.ByteString.Lazy as BSL

}

%name pCsv CSV
%tokentype { (AlexPosn, T) }
%error { pErr }

%token

    comma { ($$, Comma) }
    field { (_, D $$) }
    crlf  { ($$, CRLF) }

%%

sepBy(p,q)
    : sepBy(p,q) q p { $3 : $1 }
    | p { [$1] }

R :: { R }
  : sepBy(field,comma) { reverse $1 }

CSV :: { CSV }
    : sepBy(R,crlf) { H (reverse $1) }

{

type R = [BSL.ByteString]

data CSV = H [R]

pErr :: [(AlexPosn, T)] -> a
pErr ((l,_):_) = error (show l ++ ": unexpected"); pErr [] = error "(eof)"

p :: BSL.ByteString -> CSV
p = pCsv . alexScanTokens

}
