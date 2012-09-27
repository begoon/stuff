call generate-grammar.cmd

set CC=cl
call "%VS08COMNTOOLS%\vsvars32.bat" 
set SRC=^
 parser_grammar.c ^
 tokenizer.yy.cc ^
 tscalc.cpp ^
 executor.cpp ^
 expression.cpp ^
 function.cpp ^
 parser.cpp ^
 runtime.cpp ^
 tick_stream.cpp

set TEST=^
 tscalc_unittest.cpp ^
 executor_unittest.cpp ^
 expression_unittest.cpp ^
 function_unittest.cpp ^
 parser_unittest.cpp ^
 runtime_unittest.cpp ^
 tokenizer_unittest.cpp

%CC% /EHsc /TP /I. tscalc_unittest_runner.cpp %SRC% %TEST% gtest/gtest-all.cc
