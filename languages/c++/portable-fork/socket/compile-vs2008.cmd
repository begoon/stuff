call cl2008.cmd
cl ^
	/FeMain_vs2008.exe /MP4 /EHsc /DWIN32 ^
	Main.cpp ..\Socket.cpp ..\ListenSocket.cpp ..\SystemMessage.cpp ..\Thread.cpp ^
	wsock32.lib 