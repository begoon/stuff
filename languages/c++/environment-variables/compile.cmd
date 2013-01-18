call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat" amd64 && ^
cl /DWINDOWS /EHsc ^
  EnvironmentVariablesManager_unittest.cpp ^
  EnvironmentVariablesManager.cpp && ^
EnvironmentVariablesManager_unittest && ^
del *.obj

