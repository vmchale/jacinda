HS_SRC := $(shell find src -type f) jacinda.cabal

JAC_SRC := $(shell find prelude lib -type f)

man/ja.1: man/MANPAGE.md
	pandoc $< -s -t man -o $@

docs: doc/guide.pdf doc/guide.html docs/index.html

bins: bin/x86_64-linux-ja \
    bin/arm-linux-gnueabihf-ja \
    bin/aarch64-linux-ja \
    bin/powerpc64le-linux-ja

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
	rm -rf dist-newstyle moddeps.svg doc/guide.html *.hp *.prof bench/data bin

moddeps.svg: $(HS_SRC)
	graphmod -i src | dot -Tsvg -o $@

bin/x86_64-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	mold -run cabal build exe:ja --builddir=dist-newstyle/x86-linux --enable-executable-static
	export BIN=$$(fd 'x86_64-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    strip $@

bin/arm-linux-gnueabihf-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc arm-linux-gnueabihf-ghc-9.2.2 --with-ghc-pkg arm-linux-gnueabihf-ghc-pkg-9.2.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/arm-linux
	export BIN=$$(fd 'arm-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    arm-linux-gnueabihf-strip $@

bin/aarch64-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc aarch64-linux-gnu-ghc-9.2.2 --with-ghc-pkg aarch64-linux-gnu-ghc-pkg-9.2.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/aarch64-linux
	export BIN=$$(fd 'aarch64-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    aarch64-linux-gnu-strip $@

bin/powerpc64le-linux-ja: $(HS_SRC)
	@mkdir -p $(dir $@)
	@cabal build --with-ghc powerpc64le-linux-gnu-ghc-9.2.2 --with-ghc-pkg powerpc64le-linux-gnu-ghc-pkg-9.2.2 --project-file cabal.project.cross exe:ja --enable-executable-static --builddir=dist-newstyle/powerpc64le-linux
	export BIN=$$(fd 'ppc64-linux.*ja$$' dist-newstyle -t x -p -I); \
	    cp $$BIN $@ ; \
	    powerpc64le-linux-gnu-strip $@

tags: $(JAC_SRC)
	fd '.jac$$' prelude lib -x ja run examples/tags.jac -i > $@

bench/data/:
	mkdir -p $@

bench/data/span.txt: examples/span.txt $(dir $@)
	perl -0777pe '$$_=$$_ x 10000' $^ > $@

bench/data/ulysses.txt: $(dir $@)
	curl https://www.gutenberg.org/files/4300/4300-0.txt -o $@

check:
	fd .jac examples/ -x ja tc
