module Main (main) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Semigroup       ((<>))
import qualified Data.Version         as V
import           Jacinda.File
import           Options.Applicative
import qualified Paths_jacinda        as P
import           System.IO            (stdin)

data Command = TypeCheck !FilePath ![FilePath]
             | Run !FilePath !(Maybe FilePath) ![FilePath]
             | Expr !BSL.ByteString !(Maybe FilePath) !(Maybe BS.ByteString) ![FilePath]
             | Eval !BSL.ByteString

jacFile :: Parser FilePath
jacFile = argument str
    (metavar "JACFILE"
    <> help "Source code"
    <> jacCompletions)

jacFs :: Parser (Maybe BS.ByteString)
jacFs = optional $ option str
    (short 'F'
    <> metavar "REGEXP"
    <> help "Field separator")

jacExpr :: Parser BSL.ByteString
jacExpr = argument str
    (metavar "EXPR"
    <> help "Jacinda expression")

inpFile :: Parser (Maybe FilePath)
inpFile = optional $ option str
    (short 'i'
    <> metavar "DATAFILE"
    <> help "Data file")

jacCompletions :: HasCompleter f => Mod f a
jacCompletions = completer . bashCompleter $ "file -X '!*.jac' -o plusdirs"

commandP :: Parser Command
commandP = hsubparser
    (command "tc" (info tcP (progDesc "Type-check file"))
    <> command "e" (info eP (progDesc "Evaluate an expression (no file context)"))
    <> command "run" (info runP (progDesc "Run from file")))
    <|> exprP
    where
        tcP = TypeCheck <$> jacFile <*> includes
        runP = Run <$> jacFile <*> inpFile <*> includes
        exprP = Expr <$> jacExpr <*> inpFile <*> jacFs <*> includes
        eP = Eval <$> jacExpr

includes :: Parser [FilePath]
includes = many $ strOption
    (metavar "DIR"
    <> long "include"
    <> short 'I'
    <> dirCompletions)

dirCompletions :: HasCompleter f => Mod f a
dirCompletions = completer . bashCompleter $ "directory"


wrapper :: ParserInfo Command
wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Jacinda language for functional stream processing, filtering, and reports"
    <> header "Jacinda - a functional complement to AWK")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper

run :: Command -> IO ()
run (TypeCheck fp is)         = tcIO is =<< BSL.readFile fp
run (Run fp Nothing is)       = do { contents <- BSL.readFile fp ; runOnHandle is contents Nothing stdin }
run (Run fp (Just dat) is)    = do { contents <- BSL.readFile fp ; runOnFile is contents Nothing dat }
run (Expr eb Nothing fs is)   = runOnHandle is eb fs stdin
run (Expr eb (Just fp) fs is) = runOnFile is eb fs fp
run (Eval e)                  = print (exprEval e)
