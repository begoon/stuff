all:
ifeq ($(OS),Windows_NT)
	call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat" amd64 && \
	cl /DWINDOWS /EHsc snake.cpp
else
	g++ -o snake snake.cpp
endif

clean:
ifneq ($(OS),Windows_NT)
	-rm snake
endif

