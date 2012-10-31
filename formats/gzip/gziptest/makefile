.SILENT: clean

ifeq ($(PLATFORM_BITTAGE), 64)
VSNETINIT ?= "C:/Program Files (x86)/Microsoft Visual Studio 8/SDK/v2.0/Bin/sdkvars.bat"
VSCOMMONINIT = "C:/Program Files (x86)/Microsoft Visual Studio .NET 2003/Common7/Tools/vsvars32.bat"
else
VSNETINIT ?= "C:/Program Files/Microsoft Visual Studio 8/SDK/v2.0/Bin/sdkvars.bat"
VSCOMMONINIT = "C:/Program Files/Microsoft Visual Studio .NET 2003/Common7/Tools/vsvars32.bat"
endif

JAVA_HOME ?= C:\Program Files\Java\jdk1.5.0_15
JAVA = "$(JAVA_HOME)/bin/java"
JAVAC = "$(JAVA_HOME)/bin/javac"

ZLIB = $(P4SRCROOT)\Tools\zlib\zlib-1.2.3

TARGET = ZLibTest

orig_testfile = ZLibTest.sln
testfile = testfile

CC = @$(VSCOMMONINIT) 2>&1 1>nul && cl /nologo -I $(ZLIB)\include
LINK = @$(VSCOMMONINIT) 2>&1 1>nul && link /nologo

.PHONY: test

build-cs: Deflate.exe Inflate.exe 

build-java: Deflate.class Inflate.class

build-cpp: deflate-cpp.exe inflate-cpp.exe

build: build-cs build-java build-cpp

%.exe: %.cs
	@$(VSNETINIT) 2>&1 1>nul && csc /nologo $<

%.class: %.java
	$(JAVAC) $<

%.obj: %.cpp
	$(CC) -c $<

deflate-cpp.exe: deflate-cpp.obj
	$(LINK) /NODEFAULTLIB:LIBC $< $(ZLIB)\build\WIN32\32\zlib.lib
	
inflate-cpp.exe: inflate-cpp.obj
	$(LINK) /NODEFAULTLIB:LIBC $< $(ZLIB)\build\WIN32\32\zlib.lib
	
solution:
	@$(VSNETINIT) 2>&1 >nul && devenv.exe $(TARGET).sln
	
test: build \
		test-cs test-java test-cpp test-php \
		test-php-cpp test-cpp-php \
		test-php-java test-java-php \
		test-cpp-java test-java-cpp \
		test-cs-php test-php-cs \
		test-cs-java test-java-cs \
		test-cs-cpp test-cpp-cs

cmp = @fc /b $(orig_testfile) $(testfile).unpacked >nul || echo FAILED!

zlib:
	@$(VSCOMMONINIT) 2>&1 1>nul && cd $(ZLIB) && @nmake /C /S /f win32\Makefile.msc zlib.lib

def_args = <$(orig_testfile) >$(testfile).packed
inf_args = <$(testfile).packed >$(testfile).unpacked

def_php = @php deflate.php $(def_args)
inf_php = @php inflate.php $(inf_args)

def_java = @$(JAVA) Deflate $(def_args)
inf_java = @$(JAVA) Inflate $(inf_args)

def_c = @deflate-cpp.exe $(def_args)
inf_c = @inflate-cpp.exe $(inf_args)

def_cs = @deflate.exe $(def_args)
inf_cs = @inflate.exe $(inf_args)

test-php:
	@echo $@
	-$(def_php) && $(inf_php) &&	$(cmp)
	
test-php-java: build-java
	@echo $@
	-$(def_php) && $(inf_java) && $(cmp)
	
test-java-php: build-java
	@echo $@
	-$(def_java) && $(inf_php) && $(cmp)
	
test-cpp-php: build-cpp
	@echo $@
	-$(def_c) && $(inf_php) && $(cmp)
	
test-php-cpp: build-cpp
	@echo $@
	-$(def_php) && $(inf_c) && $(cmp)
	
test-cpp: build-cpp
	@echo $@
	-$(def_c) &&	$(inf_c) &&	$(cmp)
		
test-cpp-java: build-cpp build-java
	@echo $@
	-$(def_c) &&	$(inf_java) && $(cmp)
	
test-java-cpp: build-cpp build-java
	@echo $@
	-$(def_java) && $(inf_c) && $(cmp)
	
test-java: build-java	
	@echo $@
	-$(def_java) && $(inf_java) && $(cmp)

test-cs: build-cs
	@echo $@
	-$(def_cs) && $(inf_cs) && $(cmp)
	
test-php-cs:
	@echo $@
	-$(def_php) && $(inf_cs) && $(cmp)
	
test-cs-php:
	@echo $@
	-$(def_cs) && $(inf_php) && $(cmp)
	
test-cpp-cs:
	@echo $@
	-$(def_c) && $(inf_cs) && $(cmp)
	
test-cs-cpp:
	@echo $@
	-$(def_cs) && $(inf_c) && $(cmp)
	
test-java-cs:
	@echo $@
	-$(def_java) && $(inf_cs) && $(cmp)
	
test-cs-java:
	@echo $@
	-$(def_cs) && $(inf_java) && $(cmp)


clean:
	-@del /S /Q /F *.exe *.dll *.pdb *.cache *.suo *.class *.obj
	-@del /S /Q /F /A:H *.suo
	-@$(ComSpec) /c rmdir /S /Q bin obj
	-@del $(testfile).*
