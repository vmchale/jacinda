{ name = "jacinda"
, displayName = "Jacinda Syntax Highlighting for VSCode"
, publisher = "vmchale"
, version = "0.1.3"
, engines.vscode = "^1.0.0"
, description = "Syntax Highlighting for Jacinda"
, categories = [ "Programming Languages" ]
, keywords = [ "language", "jacinda", "highlight", "syntax" ]
, license = "AGPL3"
, homepage = "https://github.com/vmchale/jacinda"
, repository = { type = "git", url = "https://github.com/vmchale/jacinda.git" }
, bugs.url = "https://github.com/vmchale/jacinda/issues"
, contributes =
  { languages =
    [ { id = "jacinda"
      , aliases = [ "jacinda", "Jacinda" ]
      , extensions = [ ".jac" ]
      , configuration = "./language-configuration.json"
      }
    ]
  , grammars =
    [ { language = "jacinda"
      , scopeName = "source.jacinda"
      , path = "./syntaxes/jacinda.tmLanguage.json"
      }
    ]
  }
}
