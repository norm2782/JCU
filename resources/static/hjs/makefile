LANGUAGE_DIR = /Users/alessandro/Documents/Uni/xp/uhc-jscript/uhc-jscript/src

all: build

build:
	${UHC} --import-path=${LANGUAGE_DIR} -tjscript --no-recomp --no-hi-check -O,2 --dump-core-stages=1 jcu.hs
	
test:
	echo $(GHC_OPTS)
