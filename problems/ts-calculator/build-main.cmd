set CC=cl
call "%VS100COMNTOOLS%\vsvars32.bat" 
set OBJ=^
 tscalc.obj ^
 parser_grammar.obj ^
 tokenizer.yy.obj ^
 executor.obj ^
 expression.obj ^
 function.obj ^
 parser.obj ^
 runtime.obj ^
 tick_stream.obj

%CC% /EHsc /TP /I. /Fetscalc.exe tscalc_main.cpp /link %OBJ%
