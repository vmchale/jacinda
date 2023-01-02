PKG_VER:=$(shell echo '(./package.dhall).version' | dhall text)

jacinda-$(PKG_VER).vsix: syntaxes/jacinda.tmLanguage.json language-configuration.json package.json
	vsce package

syntaxes/jacinda.tmLanguage.json: syntaxes/jacinda.dhall
	dhall-to-json --file $^ --output $@

%.json: %.dhall
	dhall-to-json --file $^ --output $@

clean:
	rm -rf *.json syntaxes/*.json tags *.vsix
