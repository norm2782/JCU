LANGUAGE_DIR = /Users/norm2782/src/uhc-jscript/uhc-jscript/src
COMPILER = ${UHC} --import-path=${LANGUAGE_DIR} -tjscript --no-recomp --no-hi-check -O,2 --dump-core-stages=1

all: build

build:
	${COMPILER} jcu.hs
	
testcase: testcases/*.hs
	cd testcases && ${COMPILER} alert.hs
	
test:
	echo $(GHC_OPTS)
