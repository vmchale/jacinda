{
    module L ( AlexPosn (..)
             , T (..)
             , alexScanTokens
             ) where

import qualified Data.ByteString.Lazy as BSL

}

%wrapper "posn-bytestring"

$textdatum = [\x20-\x21 \x23-\x2b \x2d-\x7e]
$dq = \x22
@dq = $dq{2}
$lf = \x0a
$comma = \x2c
$cr =\x0d
@field = ($textdatum* | ($dq ([$textdatum $comma $cr $lf] | @dq)* $dq))

tokens :-
  
    $comma  { \p _ -> (p, Comma) }
    @field  { \p b -> (p, D (esc b)) }
    $cr $lf { \p _ -> (p, CRLF) }

{

data T = Comma | D BSL.ByteString | CRLF

esc :: BSL.ByteString -> BSL.ByteString
esc = undefined

}
