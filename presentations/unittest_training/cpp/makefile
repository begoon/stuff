all: compile test

test: test-compile test-run

test-compile:
	cl /I. /EHsc /Feiniparse_unittest.exe \
		runner.cpp iniparse_unittest.cpp \
		gtest\gtest-all.cc

test-run:
	iniparse_unittest.exe

compile:
	cl /EHsc /Feiniparse.exe main.cpp
