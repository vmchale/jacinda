{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (toList)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           File
import           Jacinda.Regex
import           System.IO            (hClose)
import           System.IO.Temp       (withSystemTempFile)
import           Test.Tasty
import           Test.Tasty.Golden    (goldenVsString)
import           Test.Tasty.HUnit

eio :: Maybe FilePath -- ^ Source filename
    -> T.Text
    -> Mode
    -> FilePath -- ^ Input
    -> IO BSL.ByteString
eio src e m fp =
    withSystemTempFile "JAC_TEST" $ \oϵ h -> do
        runOnFile [] src e [] m fp h *> hClose h
        BSL.readFile oϵ

ep :: T.Text
   -> Mode
   -> FilePath -- ^ Input
   -> BSL.ByteString -- ^ Expected output
   -> TestTree
ep e m fp expected = testCase (T.unpack e) $ do
    actual <- eio Nothing e m fp
    actual @?= expected <> "\n"

harnessF e m fp o = goldenVsString (T.unpack e) o $ eio undefined e m fp

harness :: FilePath -- ^ Source file
        -> Mode
        -> FilePath -- ^ Input file
        -> FilePath -- ^ Expected output
        -> TestTree
harness src m fp o =
    goldenVsString src o $ do {t <- TIO.readFile src; eio (Just src) t m fp}

main :: IO ()
main = defaultMain $
    testGroup "ja" [
        testGroup "stream"
          [ harness "examples/otool/rpath.jac" awk "test/data/otool" "test/golden/rpath.out"
          , harness "examples/otool/dllibs.jac" awk "test/data/otool" "test/golden/ldlib.out"
          , harness "examples/pubmed2tex.jac" awk "test/data/3221331.nbib" "test/golden/holland1988.bib"
          , harness "examples/pubmed2tex.jac" awk "test/data/22078126.nbib" "test/golden/gonadalsex.bib"
          , harness "test/examples/ghc-filt.jac" awk "test/data/ghc" "test/golden/ghc.out"
          , ep "[x ~* 1 /(\\d+(\\.\\d+)*)/]:?{%/Versions available:/}{[y]|>`$}"
                (AWK (Just "\\s*,\\s*") (Just "\\n[^:\\n]*:") True)
                "test/data/cabal-info"
                "0.1.0.5"
          , ep ".?{|`1 ~* 1 /([^\\?]*)/}" awk "test/data/url" "https://soundcloud.com/shitzulover07/ayesha-erotica-vacation-bible-school"
          , ep "[x+' '+y]|>(sprintf'-L%s')¨.?{|`1 ~* 1 /([^']*site-packages)/}"
                awk
                "test/data/python-site"
                "-L/Users/vanessa/Library/Python/3.13/lib/python/site-packages -L/Library/Frameworks/Python.framework/Versions/3.13/lib/python3.13/site-packages"
          , ep "[x]~.*{ix>1}{(`4 . `5)}" CSV "test/data/food-prices.csv" "(FINAL . Dollars)"
          , harnessF "{%/hs-source-dirs/}{`2}" (AWK (Just "\\s*:\\s*") Nothing False) "jacinda.cabal" "test/golden/src-dirs.out"
          , harnessF ".?{|`0 ~* 1 /^\\s*hs-source-dirs:\\s*(.*)/}" awk "jacinda.cabal" "test/golden/src-dirs.out"
          -- , harnessF "[x+' '+y]|>$0" (AWK Nothing (Just "\\n\\s*") False) "vscode/syntaxes/jacinda.tmLanguage.json" "test/golden/minify.out"
          , ep "@include'lib/prefixSizes.jac' prettyMem((+)|0.0 {ix>1}{`5:})" awk "test/data/ls" "73.82 kB"
          , ep "[y]|>{%/tags/}{`*}" (AWK (Just "/") Nothing False) "test/data/git-tags" "v1.7.4"
          , harnessF ".?{%/clang|mold|gold|GCC|GHC|rustc/}{`0 ~* 1 /^\\s*\\[[\\sa-f0-9]*\\]\\s*(.*$)/}" awk "test/data/readelf" "test/golden/compiler-version.out"
          , ep "~.{%/LANGUAGE\\s*.*\\s*#-/}{`3}" awk "src/Jacinda/Regex.hs" "OverloadedLists"
          , ep ".?{|`2 ~* 1 /(\\d+\\.\\d+)\\.\\d+/}" awk "test/data/py-ver" "3.13"
          , ep "{ix=1}{`2}" awk "test/data/ghc-pkg" "/Users/vanessa/.ghcup/ghc/9.10.1/lib/ghc-9.10.1/lib/../lib/aarch64-osx-ghc-9.10.1"
          , ep ".?{ix=1}{`0 ~* 1 /(\\d+\\.\\d+)/}" awk "test/data/r-version" "4.4"
          , harnessF "{ix=1}{[x+'\\n'+y]|>`$}" CSV "test/data/a.csv" "test/golden/csv-col.out"
          , harnessF "[x+' '+y]|> ~.{|subs /[^\\/]+\\/\\.\\.\\// '' `0}" awk "test/data/cdeps" "test/golden/mk-depends.out"
          , harnessF "(sub1 /\\s+$/ ⍬)¨$0" awk "test/data/trailingWhitespace" "test/golden/trailing-whitespace.out"
          , harnessF "{|option ⍬ [x] (`0 ~* 1 /^((\\s+\\S|\\S)*)\\s*$/)}" awk "test/data/trailingWhitespace" "test/golden/trailing-whitespace.out"
          , ep "[:|>.?{|`0 ~* 1 /less-(\\d+)\\.tar\\.gz/}" awk "test/data/download.html" "668"
          , ep "[:|>[x ~* 1 /less-(\\d+)\\.tar\\.gz/]:?$0" awk "test/data/download.html" "668"
          , ep "{%/libapple.dylib/}{`2}" awk "test/data/cabal-plan" "/Users/vanessa/dev/haskell/apple/dist-newstyle/build/aarch64-osx/ghc-9.10.1/apple-0.3.0.0/f/apple/build/apple/libapple.dylib"
          , harnessF "{|sprintf '%s\\t%s\\tcall cursor(%s,%s)' (`2.`3.`4.(splitc `5 '-').1)}" (AWK (Just "[\\s+:]") Nothing False) "test/data/fut-ctags" "test/golden/ctags.out"
          , harnessF "{ix=1}{sprintf'CREATE TABLE c (%s);'([x+', '+y]|>[sprintf '%s TEXT' x]¨`$)}" CSV "test/data/food-prices.csv" "test/golden/sql.out"
          , harnessF "{%/infix(r|l)? \\d+/}{sprintf '- fixity: %s' `0}" awk "src/A.hs" "test/golden/hlint-fix.out"
          , harnessF "{%/^PATH/}{`2}" (AWK (Just "=") Nothing False) "test/data/env" "test/golden/env.out"
          ]
      , testGroup "eval"
          [ splitWhitespaceT "" []
          , splitWhitespaceT "5" ["5"]
          , testCase "split eval" (evalTo "[x+' '+y]|> split '01-23-1987' /-/" "01 23 1987")
          , testCase "length eval" (evalTo "#*split '01-23-1987' /-/" "3")
          , testCase "captureE" (evalTo "'01-23-1987' ~* 3 /(\\d{2})-(\\d{2})-(\\d{4})/" "Some 1987")
          , testCase "subs" (evalTo "subs /zi/ '.' 'vectorzm0zi13zi1zi0zmc80ea02f780be2984f831df2de071f6e6040c0f670b3dd2428e80f5d111d7f72_DataziVectorziGeneric_partition_closure'" "vectorzm0.13.1.0zmc80ea02f780be2984f831df2de071f6e6040c0f670b3dd2428e80f5d111d7f72_Data.Vector.Generic_partition_closure")
          , testCase "basename" (evalTo "'test/data/py.py' ~* 2 /([^\\/]*\\/)*(.*)/" "Some py.py")
          ]
    ]

evalTo :: T.Text -> String -> Assertion
evalTo bsl expected =
    let actual = show (exprEval bsl)
        in actual @?= expected

splitWhitespaceT :: BS.ByteString -> [BS.ByteString] -> TestTree
splitWhitespaceT haystack expected =
    testCase "split col" $
        toList (splitBy defaultRurePtr haystack) @?= expected
