call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat" amd64 && ^
cl /Ox /DWIN32 sort_async.cpp && ^
sort_async generate
