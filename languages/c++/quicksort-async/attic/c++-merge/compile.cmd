call "%VS110COMNTOOLS%..\..\VC\vcvarsall.bat" amd64 && ^
cl /Ox /DWIN32 sort_benchmark.cpp && ^
sort_benchmark
