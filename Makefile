HS_SRC := $(shell find src -type f) jacinda.cabal

JAC_SRC := $(shell find prelude lib -type f)

man/ja.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

docs: doc/guide.pdf doc/guide.html docs/index.html

docs/index.html: doc/guide.html
	cp $^ $@

doc/guide.pdf: doc/guide.md
	pandoc $^ -o $@ --toc

doc/guide.html: doc/guide.md
	pandoc -s $^ -o $@ --toc

install: man/ja.1
	cabal install exe:ja --overwrite-policy=always -w ghc-8.10.7
	strip $$(which ja)
	cp man/ja.1 $(HOME)/.local/share/man/man1

clean:
	rm -rf dist-newstyle moddeps.svg doc/guide.html *.hp *.prof

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

tags: $(JAC_SRC)
	fd '.jac$$' prelude lib -x ja run examples/tags.jac -i > $@

bench/data/span.txt: examples/span.txt
	mkdir -p $(dir $@)
	perl -0777pe '$$_=$$_ x 10000' $^ > $@
