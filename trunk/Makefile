default:
	make build && make run

build:
	make clean
	ghc --make GUI.hs
	macosx-app ./GUI

clean:
	rm -rf GUI GUI.app/ *.hi *.o

run:
	open ./GUI.app
