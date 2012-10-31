#include <gtest/gtest.h>

#include "Socket.h"
#include "Delay.h"

#include "Runner.h"

#include <string>
#include <iostream>

TEST(SocketTest, InternetConnection) {
	monitor::Socket peer;

	try {
		peer.connect("gvamail.temenos.com", 80);

		peer.write("GET / HTTP/1.0\n\n");

		std::string buf;
		do {
			std::string chunk = peer.read(monitor::Socket::FastBufferSize);
			ASSERT_TRUE(chunk.length() > 0) << "Empty buffer after blocking read()";
			buf.append(chunk);
			if (chunk.length() < monitor::Socket::FastBufferSize) break;
		} while(1);

		EXPECT_TRUE(buf.find("HTTP/1") != std::string::npos);

	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, CharGeneratorLongFastBuffer) {
	monitor::Socket peer;

	try {
		peer.connect(testhost, 19);

		volatile char buf[monitor::Socket::FastBufferSize];
		for (int i = 0; i < 10000 * 1024; i += sizeof(buf)) {
			peer.read((char *)buf, sizeof(buf));
		}
	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, CharGeneratorLongSlowBuffer) {
	monitor::Socket peer;

	try {
		peer.connect(testhost, 19);

		char buf[10240];
		for (int i = 0; i < 10000 * 1024; i += sizeof(buf)) {
			peer.read((char *)buf, sizeof(buf));
		}
	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, CharGeneratorLongFastString) {
	monitor::Socket peer;

	try {
		peer.connect(testhost, 19);

		for (int i = 0; i < 10000 * 1024; i += monitor::Socket::FastBufferSize) {
			volatile std::string chunk = peer.read(monitor::Socket::FastBufferSize);
		}
	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, CharGeneratorLongSlowString) {
	monitor::Socket peer;

	try {
		peer.connect(testhost, 19);

		for (int i = 0; i < 10000 * 1024; i += 10240) {
			volatile std::string chunk = peer.read(10240);
		}
	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, CharGenerator) {
	monitor::Socket peer;

	try {
		std::string expected(" !\"#$%&'");

		peer.connect(testhost, 19);

		std::string actual = peer.read(expected.length());
		EXPECT_EQ(expected, actual);

	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, Echo) {
	monitor::Socket peer;

	try {
		std::string expected("ECHOTEST");

		peer.connect(testhost, 7);
		peer.write("ECHOTEST");

		std::string actual = peer.read(expected.length());
		EXPECT_EQ(expected, actual);

	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, EchoAvail) {
	monitor::Socket peer;

	try {
		std::string expected("ECHOTEST");

		peer.connect(testhost, 7);
		peer.write("ECHOTEST");

		while (peer.avail() < expected.length()) {
			msleep(1);
		}

		EXPECT_EQ(expected.length(), peer.avail());

	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, UseCreatedHandle) {
	monitor::Socket parent;

	monitor::Socket peer(parent.handle());
	EXPECT_TRUE(peer.connected());

	try {
		std::string expected("ECHOTEST");

		peer.connect(testhost, 7);
		peer.write("ECHOTEST");

		std::string actual = peer.read(expected.length());
		EXPECT_EQ(expected, actual);

	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, Connected) {
	monitor::Socket peer;

	EXPECT_FALSE(peer.connected());

	try {
		peer.connect(testhost, 7);
		EXPECT_TRUE(peer.connected());
		peer.disconnect();
		EXPECT_FALSE(peer.connected());
	} catch (monitor::SocketError& e) {
		FAIL() << e.what();
	}
}

TEST(SocketTest, UnknownHost) {
	monitor::Socket peer;

	try {
		peer.connect("localhost0", 54321);
		FAIL() << "Connect to localhost0:54321 must fail";
	} catch (monitor::Socket::ConnectFailure& e) {
		std::string expected("localhost0:54321");
		EXPECT_EQ(expected, std::string(e.what()).substr(0, expected.length()));
	}
}

TEST(SocketTest, BadConnect) {
	monitor::Socket peer;

	try {
		peer.connect("127.0.0.1", 54321);
		FAIL() << "Connect to 127.0.0.1:54321 must fail";
	} catch (monitor::Socket::ConnectFailure& e) {
		std::string expected("127.0.0.1:54321");
		EXPECT_EQ(expected, std::string(e.what()).substr(0, expected.length()));
	}				
}
