default:
	make clean
	cabal install

jcu:
	make && jcu

dist:
	cabal check
	cabal configure
	cabal sdist

dev:
	cabal install -fdevelopment --disable-documentation
	
clean:
	cabal clean

run:
	jcu

debug:
	DEBUG=1 jcu

uninstall:
	ghc-pkg unregister jcu
	rm ~/Library/Haskell/bin/jcu
	rm -rf ~/Library/Haskell/ghc-7.0.3/lib/JCU-*
