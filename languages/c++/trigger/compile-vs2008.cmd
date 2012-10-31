call cl2008.cmd
cl /EHsc /I. /Fetrigger_unittest_vs2008.exe /DWIN32 trigger.cpp runner.cpp trigger_unittest.cpp pretimer_unittest.cpp pretimer.cpp thread.cpp gtest\gtest-all.cc
