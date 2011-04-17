default:
	make install

dist:
	cabal check
	cabal configure
	cabal sdist

install:
	cabal install -fdevelopment
	
clean:
	cabal clean

run:
	jcu

debug:
	DEBUG=1 jcu
