set GOMAXPROCS=4
set GO=c:\go\bin

%GO%\gofmt.exe -tabs=false -tabwidth=2 -w=true sort.go
%GO%\go.exe run sort.go
