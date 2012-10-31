.SILENT: clean

JAVA_HOME ?= C:\Program Files\Java\jdk1.5.0_15
JAVA = "$(JAVA_HOME)/bin/java"
JAVAC = "$(JAVA_HOME)/bin/javac"

testsource ?= c:\windows\system32\mspaint.exe
testfile = testfile

ifile ?= EMPTY
ofile ?= EMPTY

JBASEPATH = com/jbase/framework/io

build: build-java

build-java: build-java-core Deflate.class Inflate.class

build-java-core: $(JBASEPATH)/Compressor.class $(JBASEPATH)/GZipCompressor.class

%.class: %.java
	$(JAVAC) $<

test: build \
		test-java

deflate:
	@$(JAVA) Deflate <$(ifile) >$(ofile)
	
inflate:
	@$(JAVA) Inflate <$(ifile) >$(ofile)
	
init = @copy $(testsource) $(testfile).original >nul
cmp = @fc /b $(testfile).original $(testfile).unpacked >nul || echo FAILED!

def_args = <$(testfile).original >$(testfile).packed
inf_args = <$(testfile).packed >$(testfile).unpacked

def_java = @$(JAVA) Deflate $(def_args)
inf_java = @$(JAVA) Inflate $(inf_args)

test-java: build-java	
	@echo $@
	-$(init) && $(def_java) && $(inf_java) && $(cmp)

clean:
	-@del /S /Q /F *.exe *.dll *.pdb *.cache *.suo *.class *.obj
	-@del /S /Q /F /A:H *.suo
	-@$(ComSpec) /c rmdir /S /Q bin obj
	-@del $(testfile).*
	-@del $(JBASEPATH)/*.class
