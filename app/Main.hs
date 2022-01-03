module Main (main) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Version         as V
import           Jacinda.File
import           Options.Applicative
import qualified Paths_jacinda        as P
import           System.IO            (stdin)

data Command = TypeCheck !FilePath
             | Run !FilePath !(Maybe FilePath)
             | Expr !BSL.ByteString !(Maybe FilePath)

jacFile :: Parser FilePath
jacFile = argument str
    (metavar "JACFILE"
    <> help "Source code"
    <> jacCompletions)

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
    <> command "run" (info runP (progDesc "Run from file")))
    <|> exprP
    where
        tcP = TypeCheck <$> jacFile
        runP = Run <$> jacFile <*> inpFile
        exprP = Expr <$> jacExpr <*> inpFile

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Jacinda language for functional stream processing, filtering, and reports"
    <> header "Jacinda - a functional AWK")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper

run :: Command -> IO ()
run (TypeCheck fp)      = tcIO =<< BSL.readFile fp
run (Run fp Nothing)    = do { contents <- BSL.readFile fp ; runOnHandle contents stdin }
run (Run fp (Just dat)) = do { contents <- BSL.readFile fp ; runOnFile contents dat }
run (Expr eb Nothing)   = runOnHandle eb stdin
run (Expr eb (Just fp)) = runOnFile eb fp
