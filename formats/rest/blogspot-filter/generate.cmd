set SRC=%1
call %~dp0compile-html.cmd %SRC%
set PYTHON=c:\python27
%PYTHON%\python %~dp0blogspot-filter.py <%SRC%.html >%SRC%.filtered.html
