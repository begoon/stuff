all: compile test

compile:
	cl /Zi iniparse.c main.c

test: test-compile test-run

test-compile:
	cl /Zi /DUNITTESTING /I. \
		iniparse_unittest_runner.c iniparse.c cmockery.c

test-run:
	iniparse_unittest_runner
