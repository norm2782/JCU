LANGUAGE_DIR = /Users/norm2782/src/uhc-jscript/uhc-jscript/src

all: build

build:
	${UHC} --import-path=${LANGUAGE_DIR} -tjscript --no-recomp --no-hi-check -O,2 --dump-core-stages=1 jcu.hs
