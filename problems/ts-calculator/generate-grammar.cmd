set CYGWIN_HOME=c:\cygwin
call "%VS100COMNTOOLS%\vsvars32.bat" && cl /EHsc lemon.c
%CYGWIN_HOME%\bin\flex.exe -otokenizer.yy.cc tokenizer.l
lemon parser_grammar.y
