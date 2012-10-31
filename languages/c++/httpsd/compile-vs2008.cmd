call cl2008.cmd
cl ^
	%1 ^
	/Zi /FeSSL_vs2008.exe /MD /MP4 /EHsc /DWIN32 ^
	/Igtest\include /Igtest /Iopenssl\include ^
	Mutex_unittest.cpp ^
	ListenSocket.cpp ListenSocket_unittest.cpp ^
	ListenSocketThread.cpp ^
	Socket.cpp Socket_unittest.cpp ^
	SSLSocket.cpp SSLContext.cpp ^
	SSLListenSocket.cpp ^
	SSLError.cpp ^
	SSL_unittest.cpp ^
	SystemMessage.cpp ^
	Thread.cpp Thread_unittest.cpp ^
	Runner.cpp ^
	gtest\src\gtest-all.cpp ^
	openssl\lib\win32\libeay32.lib openssl\lib\win32\ssleay32.lib ^
	wsock32.lib gdi32.lib


