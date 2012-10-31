HOST = -h hmll0803001.europe.temenosgroup.com

SRC = \
	Mutex_unittest.cpp \
	ListenSocket.cpp ListenSocket_unittest.cpp \
	ListenSocketThread.cpp \
	Socket.cpp Socket_unittest.cpp \
	SSLSocket.cpp SSLContext.cpp \
	SSLListenSocket.cpp \
	SSLError.cpp \
	SSL_unittest.cpp \
	SystemMessage.cpp \
	Thread.cpp Thread_unittest.cpp \
	Runner.cpp \
	gtest/src/gtest-all.cpp

INC = -Igtest/include -Igtest -Iopenssl/include \

TARGET = SSL_unittest

all:

linux: linux-build run

sunos: sunos-build run

aix: aix-build run

hpux: hpux-build run

linux-build:
	g++ \
		$(opt) \
		-DLINUX \
		-g \
		-o $(TARGET) \
		$(SRC) \
		$(INC) \
		-lssl -lpthread

sunos-build:
	CC \
		$(opt) \
		-xtarget=ultra -xarch=v9 \
		-DSUN \
		-g \
		-o $(TARGET) \
		$(SRC) \
		$(INC) \
		-lssl -lcrypto -lnsl -lsocket

aix-build:
	xlC_r \
		$(opt) \
		$(INC) \
		-o $(TARGET) \
		-brtl -q64 -qrtti=all -qthreaded -qeh \
		$(SRC) \
		-lssl -lcrypto -lpthread \
		-Lopenssl/lib/AIX5

hpux-build:
	aCC \
		-DNO_STRERROR_R -DHPUX -D_XOPEN_SOURCE_EXTENDED \
		$(opt) \
		$(INC) \
		-o $(TARGET) \
		$(SRC) \
		-lssl -lcrypto \
		-Lopenssl/lib/HPUX_IA64
run:
	./$(TARGET) $(HOST) $(opt)
