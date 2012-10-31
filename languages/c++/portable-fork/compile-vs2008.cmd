call cl2008.cmd
cl ^
	/Zi /FeMain_vs2008.exe /MP4 /EHsc /DWIN32 ^
	-Igtest\include -Igtest ^
	Main.cpp SocketMultiProcessListener.cpp ListenSocket.cpp Socket.cpp SystemMessage.cpp Thread.cpp ^
	SocketMultiProcessListener_unittest.cpp ^
	Logger.cpp ^
	gtest\src\gtest-all.cpp ^
	wsock32.lib 