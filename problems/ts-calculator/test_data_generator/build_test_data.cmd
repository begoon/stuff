call compile_test_generator.cmd
set CMD=test_data_generator
set DIR=..\data
set SYMBOLS=eur bid ask gbp
for %%I in (%SYMBOLS%) do %CMD% <plain_%%~nI.data %DIR%\%%~nI

