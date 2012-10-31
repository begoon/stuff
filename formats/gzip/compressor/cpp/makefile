.SILENT: clean

ifeq ($(PLATFORM_BITTAGE), 64)
VSNETINIT ?= "C:/Program Files (x86)/Microsoft Visual Studio 8/SDK/v2.0/Bin/sdkvars.bat"
VSCOMMONINIT = "C:/Program Files (x86)/Microsoft Visual Studio .NET 2003/Common7/Tools/vsvars32.bat"
else
VSNETINIT ?= "C:/Program Files/Microsoft Visual Studio 8/SDK/v2.0/Bin/sdkvars.bat"
VSCOMMONINIT = "C:/Program Files/Microsoft Visual Studio .NET 2003/Common7/Tools/vsvars32.bat"
endif

testsource ?= c:\windows\system32\mspaint.exe
testfile = testfile

ifile ?= EMPTY
ofile ?= EMPTY

.PHONY: test

build: deflator.exe inflator.exe 

%.exe: %.sln
	@$(VSCOMMONINIT) 2>&1 1>nul && devenv $< /nologo /Build Debug /Out $@.errors

deflate:
	@deflator <$(ifile) >$(ofile)
	
inflate:
	@inflator <$(ifile) >$(ofile)

test: build \
		test-cpp

init = @copy $(testsource) $(testfile).original >nul
cmp = @fc /b $(testfile).original $(testfile).unpacked >nul || echo FAILED!

def_args = <$(testfile).original >$(testfile).packed
inf_args = <$(testfile).packed >$(testfile).unpacked

def_cpp = @deflator.exe $(def_args)
inf_cpp = @inflator.exe $(inf_args)

test-cpp:
	@echo $@
	-$(init) && $(def_cpp) && $(inf_cpp) && $(cmp)
	
clean:
	-@del /S /Q /F *.exe *.dll *.pdb *.cache *.suo *.class *.obj *.errors *.ilk *.ncb
	-@del /S /Q /F /A:H *.suo
	-@$(ComSpec) /c rmdir /S /Q bin obj
	-@del $(testfile).*
	-@rmdir /Q /S debug release obj
