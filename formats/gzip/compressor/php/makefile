.SILENT: clean

testsource ?= c:\windows\system32\mspaint.exe
testfile = testfile

ifile ?= EMPTY
ofile ?= EMPTY

test: test-php

init = @copy $(testsource) $(testfile).original >nul
cmp = @fc /b $(testfile).original $(testfile).unpacked >nul || echo FAILED!

def_args = <$(testfile).original >$(testfile).packed
inf_args = <$(testfile).packed >$(testfile).unpacked

def_php = @php deflate.php $(def_args)
inf_php = @php inflate.php $(inf_args)

build:

deflate:
	@php deflate.php <$(ifile) >$(ofile)
	
inflate:
	@php inflate.php <$(ifile) >$(ofile)
	
test-php:
	@echo $@
	-$(init) && $(def_php) && $(inf_php) && $(cmp)

clean:
	-@del /S /Q /F *.exe *.dll *.pdb *.cache *.suo *.class *.obj
	-@del /S /Q /F /A:H *.suo
	-@$(ComSpec) /c rmdir /S /Q bin obj
	-@del $(testfile).*
