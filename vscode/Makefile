all: syntaxes/jacinda.tmLanguage.json language-configuration.json package.json

syntaxes/jacinda.tmLanguage.json: syntaxes/jacinda.dhall
	dhall-to-json --file $^ --output $@

%.json: %.dhall
	dhall-to-json --file $^ --output $@

clean:
	rm -rf *.json syntaxes/*.json tags
