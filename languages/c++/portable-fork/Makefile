SRC = Main.cpp \
	SocketMultiProcessListener.cpp ListenSocket.cpp Socket.cpp SystemMessage.cpp Thread.cpp \
	SocketMultiProcessListener_unittest.cpp \
	gtest/src/gtest-all.cpp

INC = -Igtest -Igtest/include

OBJ = $(patsubst %.cpp,%.o,$(SRC))

all:
	@echo Targets available: 
	@echo - hpux, sunos, aix, linux
	@echo - run, clean

hpux:
	aCC $(INC) -g -mt -lpthread -DHPUX -DNO_STRERROR_R -o Main $(SRC)

sunos:
	CC $(INC) -g -lsocket -lnsl -o Main $(SRC)

aix:
	xlC $(INC) -g -lpthread -o Main $(SRC)

linux:
	c++ $(INC) -g -lpthread -o Main $(SRC)

run:
	./Main

clean:
	-rm $(OBJ) Main
