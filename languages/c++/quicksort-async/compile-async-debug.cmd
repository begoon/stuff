call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat" amd64 && ^
cl /Zi /DWIN32 sort_async.cpp && ^
sort_async
