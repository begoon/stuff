#include <iostream>

#include "..\Socket.h"
#include "..\ListenSocket.h"

int main(int argc, char* argv[])
{
	monitor::ListenSocket listener;

	listener.listen(30000);

	while (true) {
		monitor::Socket* peer = listener.accept();

		peer->write("OK\n", 3);

		if (!peer->select()) {
			std::cout << "select(): " << peer->error_message() << std::endl;
			break;
		}

		char buf[1024];
		if (peer->read(buf, 2) != 2) {
			std::cout << "read(): " << peer->error_message() << std::endl;
			break;
		}

		buf[2] = 0;
		std::cout << "[" << buf << "]" << std::endl;

		peer->shutdown();

		delete peer;
	}

	return 0;
}
