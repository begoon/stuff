#include "gtest/gtest.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <string>

#include "Thread.h"
#include "Socket.h"

#include "SocketMultiProcessListener_unittest.h"

void checkDefunct() {
	std::string defunct;
	
#ifndef WIN32
	FILE* chld = popen("ps -ef | grep -v grep | grep Main | grep -i defunct", "r");

	while (!feof(chld)) {
		char buf[1024];
		int sz = fread(reinterpret_cast<void *>(buf), 1, sizeof(buf), chld);
		if (sz > 0)
			defunct += buf;
	}

	fclose(chld);
#endif

	EXPECT_EQ(0, defunct.length()) << "Zombies found: " << std::endl << defunct;
}

void checkChildExistance(const SocketMultiProcessListener* client) {
	std::string alive;
#ifndef WIN32
	std::stringstream fmt;
	fmt << "ps -ef | grep -v grep | grep Main | grep __" << client->name();

	FILE* chld = popen(fmt.str().c_str(), "r");

	while (!feof(chld)) {
		char buf[1024];
		int sz = fread(reinterpret_cast<void *>(buf), 1, sizeof(buf), chld);
		if (sz > 0)
			alive += buf;
	}

	fclose(chld);
#endif

	EXPECT_EQ(0, alive.length()) << "Alive child found: " << std::endl << alive;
}

void sanityCheck(const SocketMultiProcessListener* client)
{
	checkDefunct();
	checkChildExistance(client);
}

TEST(Fork, SimpleClientId)
{
	EXPECT_EQ("Simple", SimpleClient().name());
	EXPECT_EQ("--child__Simple", SimpleClient().id());
}

TEST(Fork, Simple)
{
	SimpleClient* client = new SimpleClient();
	SocketMultiProcessListenerThread* listener = new SocketMultiProcessListenerThread(client, 30000);
	listener->safeStart();

	monitor::Socket sock;
	EXPECT_TRUE(sock.connect("localhost", 30000)) << sock.error_message();

	char buf[1024];

	EXPECT_EQ(client->data().length(), sock.read(buf, client->data().length()));

	EXPECT_EQ(std::string(buf, client->data().length()), client->data());

	sock.disconnect();

	sanityCheck(client);
}

TEST(Fork, Sequence)
{
	SequenceClient* client = new SequenceClient();
	(new SocketMultiProcessListenerThread(client, 30001))->safeStart();

	monitor::Socket sock;
	EXPECT_TRUE(sock.connect("localhost", 30001)) << sock.error_message();

	for (int i = 0; i < 256; i++) {
		char expected = i & 0xff, actual;

		EXPECT_EQ(1, (int)sock.read(&actual, 1));
		EXPECT_EQ(expected, actual);
	}

	sock.disconnect();

	sanityCheck(client);
}

TEST(Fork, Echo)
{
	EchoClient* client = new EchoClient();
	(new SocketMultiProcessListenerThread(client, 30002))->safeStart();

	monitor::Socket sock;
	EXPECT_TRUE(sock.connect("localhost", 30002)) << sock.error_message();

	for (int i = 0; i < 256; i++) {
		char expected = i & 0xff, actual;

		sock.write(&expected, 1);
		EXPECT_EQ(1, (int)sock.read(&actual, 1));
		EXPECT_EQ(expected, actual);
	}

	sock.disconnect();

	sanityCheck(client);
}
