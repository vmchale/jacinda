{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A                   (Mode (..))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Version        as V
import           File
import           Options.Applicative
import qualified Paths_jacinda       as P

data Command = TypeCheck !FilePath ![FilePath]
             | Run !FilePath !(Maybe T.Text) !(Maybe T.Text) !(Maybe FilePath) ![FilePath]
             | Expr !T.Text !(Maybe FilePath) !(Maybe T.Text) !Bool !Bool !Bool !(Maybe T.Text) ![FilePath]
             | Eval !T.Text

jacFile :: Parser FilePath
jacFile = argument str
    (metavar "JACFILE"
    <> help "Source code"
    <> jacCompletions)

csv :: Parser Bool
csv = switch
    (long "csv"
    <> help "Process as CSV")

asv :: Parser Bool
asv = switch
    (long "asv"
    <> help "Process as ASV")

usv :: Parser Bool
usv = switch
    (long "usv"
    <> short 'u'
    <> help "Process as USV")

jacRs :: Parser (Maybe T.Text)
jacRs = optional $ option str
    (short 'R'
    <> metavar "REGEXP"
    <> help "Record separator")

jacFs :: Parser (Maybe T.Text)
jacFs = optional $ option str
    (short 'F'
    <> metavar "REGEXP"
    <> help "Field separator")

jacExpr :: Parser T.Text
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
        runP = Run <$> jacFile <*> jacFs <*> jacRs <*> inpFile <*> includes
        exprP = Expr <$> jacExpr <*> inpFile <*> jacFs <*> asv <*> usv <*> csv <*> jacRs <*> includes
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

ap :: Bool -> Bool -> Bool -> Maybe T.Text -> Maybe T.Text -> Mode
ap True True _ _ _          = errorWithoutStackTrace "--asv and --usv both specified."
ap True _ True _ _          = errorWithoutStackTrace "--asv and --csv both specified."
ap _ True True _ _          = errorWithoutStackTrace "--usv and --csv both specified."
ap _ True _ Just{} _        = errorWithoutStackTrace "--usv and field separator both speficied."
ap _ True _ _ Just{}        = errorWithoutStackTrace "--usv and record separator both speficied."
ap True _ _ Just{} _        = errorWithoutStackTrace "--asv and field separator both speficied."
ap True _ _ _ Just{}        = errorWithoutStackTrace "--asv and record separator both speficied."
ap _ _ True Just{} _        = errorWithoutStackTrace "--csv and field separator both speficied."
ap _ _ True _ Just{}        = errorWithoutStackTrace "--csv and record separator both speficied."
ap _ _ True Nothing Nothing = CSV
ap True _ _ Nothing Nothing = AWK (Just "\\x1f") (Just "\\x1e")
ap _ True _ Nothing Nothing = AWK (Just "␟") (Just "␞")
ap _ _ _ fs rs              = AWK fs rs

run :: Command -> IO ()
run (TypeCheck fp is)                  = tcIO is =<< TIO.readFile fp
run (Run fp fs rs Nothing is)          = do { contents <- TIO.readFile fp ; runStdin is contents (AWK fs rs) }
run (Run fp fs rs (Just dat) is)       = do { contents <- TIO.readFile fp ; runOnFile is contents (AWK fs rs) dat }
run (Expr eb Nothing fs a u c rs is)   = let m = ap a u c fs rs in runStdin is eb m
run (Expr eb (Just fp) fs a u c rs is) = let m = ap a u c fs rs in runOnFile is eb m fp
run (Eval e)                           = print (exprEval e)
