all:
	cabal build
	cp dist-newstyle/build/x86_64-linux/ghc-9.4.8/gt-lang-0.1.0.0/x/gt-lang/build/gt-lang/gt-lang interpreter

clean:
	cabal clean
