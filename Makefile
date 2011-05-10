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

deps:
	git clone https://github.com/snapframework/snap-auth.git
	cd snap-auth
	cabal install
	git clone https://github.com/ozataman/snap-extension-mongodb.git
	cd snap-extension-mongodb
	cabal install
	cd ..
