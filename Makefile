default:
	make build && make run

build:
	make clean
	cd src && ghc --make GUI.hs
	cd src && macosx-app ./GUI

clean:
	rm -rf src/GUI src/GUI.app/ src/*.hi src/*.o

run:
	open src/GUI.app
