for %%I in (.) do set CWD=%%~nI
winrar u -v -r -s -ag-YYYY.MM.DD-HH.MM.SS -x*.rar -x*.lib -x*.a %CWD%
