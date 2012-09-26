call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat" amd64 && ^
cl /Ox /DWIN32 generate.cpp && ^
generate
