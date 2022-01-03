HS_SRC := $(shell find src -type f) jacinda.cabal

man/ja.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

docs: docs/guide.pdf docs/guide.html

docs/guide.pdf: docs/guide.md
	pandoc $^ -o $@ --toc

docs/guide.html: docs/guide.md
	pandoc -s $^ -o $@ --toc

install: man/ja.1
	cabal install exe:ja --overwrite-policy=always -w ghc-9.2.1
	strip $$(which ja)
	cp man/ja.1 $(HOME)/.local/share/man/man1

clean:
	rm -rf dist-newstyle moddeps.svg docs/guide.html *.hp *.prof

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@
