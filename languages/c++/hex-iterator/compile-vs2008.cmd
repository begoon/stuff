..\tools\cl2008.cmd ^
&& cl /O2 /EHsc /I..\gtest-1.4.0 /Festring_hex_unittest_vs2008.exe ^
  runner.cpp string_hex_unittest.cpp ^
  ..\gtest-1.4.0\gtest-all.cc
