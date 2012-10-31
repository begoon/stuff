#include <gtest/gtest.h>

#include "ListenSocket.h"
#include "Socket.h"
#include "Thread.h"
#include "Delay.h"
#include "ListenSocketThread.h"

class EchoClient: public monitor::ListenSocketClient {
public:
	virtual void Action(monitor::Socket* peer, monitor::ListenSocketThread* thread) {
		while (true) {
			try {
				std::string data = peer->read(1);
   			peer->write(data);

				if (data == "*") {
					thread->terminate();
					break;
				}
			} catch(monitor::SocketError& e) {
				break;
			}
		}
	}
};

TEST(ListenSocketTest, Simple) {
	int port = 40000;
	monitor::ListenSocketThread* thread = new monitor::ListenSocketThread(
		new monitor::ListenSocket(),
		new EchoClient(),
		port
	);
	thread->safeStart();		

	monitor::Socket peer;
	peer.connect("localhost", port);

	std::string expected("ECHOTEST");

	peer.write(expected);
	EXPECT_EQ(expected, peer.read(expected.length()));

	peer.write("*");

	thread->Join();
	delete thread;
}
