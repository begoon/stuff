set PATH=c:\cygwin\bin
g++ -o Main_cygwin.exe -g ^
	Main.cpp SocketMultiProcessListener.cpp ListenSocket.cpp Socket.cpp SystemMessage.cpp Thread.cpp ^
	SocketMultiProcessListener_unittest.cpp ^
	Logger.cpp ^
	-Igtest\include -Igtest ^
	gtest\src\gtest-all.cpp
