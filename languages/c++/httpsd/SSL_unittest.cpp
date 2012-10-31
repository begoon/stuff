#include <gtest/gtest.h>

#include <fstream>
#include <sstream>
#include <iostream>

#include "SSLContext.h"
#include "SSLSocket.h"
#include "SSLListenSocket.h"
#include "SSLError.h"
#include "ListenSocket.h"
#include "ListenSocketThread.h"

class SSLTest: public testing::Test {
protected:
	const char* certfname() const { return "cert.pem"; }
	const char* keyfname() const { return "key.pem"; }

	virtual void SetUp() {
		std::ofstream cert(certfname());
		cert << 
			"-----BEGIN CERTIFICATE-----\n"
			"MIIC2DCCAkGgAwIBAgIJALptEXUhC5kTMA0GCSqGSIb3DQEBBQUAMFIxCzAJBgNV\n"
			"BAYTAkFVMRMwEQYDVQQIEwpTb21lLVN0YXRlMRYwFAYDVQQKEw1NeUNvbXBhbnlO\n"
			"YW1lMRYwFAYDVQQDEw1NeUNvbXBhbnlOYW1lMB4XDTA4MDcxMDA5MzMxNFoXDTA5\n"
			"MDcxMDA5MzMxNFowUjELMAkGA1UEBhMCQVUxEzARBgNVBAgTClNvbWUtU3RhdGUx\n"
			"FjAUBgNVBAoTDU15Q29tcGFueU5hbWUxFjAUBgNVBAMTDU15Q29tcGFueU5hbWUw\n"
			"gZ8wDQYJKoZIhvcNAQEBBQADgY0AMIGJAoGBAMMcKQ/dau+wJDPaXHPndsgCKQgs\n"
			"AUMnqSw4DZoapBlWJ1J5CvEIJJIqnFHbxk9kDAz1g9zgwSMJE311hC0tRw6idSDh\n"
			"7CznKdgJaD/Hp7Y+vAK7yr6H9vHKDZA6wRhr99bjuZcqQS87mTH1QT86xwJGrttP\n"
			"CSYNiiB9NHV7jyRZAgMBAAGjgbUwgbIwHQYDVR0OBBYEFN2yRL8s0pU48FubdhJK\n"
			"gBds6zphMIGCBgNVHSMEezB5gBTdskS/LNKVOPBbm3YSSoAXbOs6YaFWpFQwUjEL\n"
			"MAkGA1UEBhMCQVUxEzARBgNVBAgTClNvbWUtU3RhdGUxFjAUBgNVBAoTDU15Q29t\n"
			"cGFueU5hbWUxFjAUBgNVBAMTDU15Q29tcGFueU5hbWWCCQC6bRF1IQuZEzAMBgNV\n"
			"HRMEBTADAQH/MA0GCSqGSIb3DQEBBQUAA4GBAHrEQf8iDUYeTO2jx/AlVsYC1nx6\n"
			"zEh1kPtdcN57hVpnIooP9NlhZBJ/Cc7DtJikpZhHU4AJdZx9O5uenS06fFdjHD0X\n"
			"1zWzI27GpT4kBwQPygexRjeb+6JopwibigmVIxE+u2dLwrQzrCOXxxuYDHgfZZmv\n"
			"WvKs6j3JlDKL3hS9\n"
			"-----END CERTIFICATE-----\n"
		;
		std::ofstream key(keyfname());
		key << 
			"-----BEGIN RSA PRIVATE KEY-----\n"
			"MIICXAIBAAKBgQDDHCkP3WrvsCQz2lxz53bIAikILAFDJ6ksOA2aGqQZVidSeQrx\n"
			"CCSSKpxR28ZPZAwM9YPc4MEjCRN9dYQtLUcOonUg4ews5ynYCWg/x6e2PrwCu8q+\n"
			"h/bxyg2QOsEYa/fW47mXKkEvO5kx9UE/OscCRq7bTwkmDYogfTR1e48kWQIDAQAB\n"
			"AoGAYlt538OROw+i0HkODmfrG1zrbnEwmbgOsnn3cW5ot1UVMaBPgb7HIOBWOXX/\n"
			"fNpEsOu788dh1dWne6GYZmXi4X5E2uLksnD9CfD356X8RjK8fX3un8XpYE64X3FF\n"
			"9+cdSJdklbSDPPGGzjkwTkAzF68hnp0uYvt4t3KrAARpRWECQQDopNSBPtKN7Fr2\n"
			"H8wNMH8fyPs4Rknt6wfWvA5CNQvI6M6EkLXpK8ae2su6TAaytcNboYW43NdFlpNd\n"
			"y6Kj8AOdAkEA1rKsjk3w8DgOrCZOhSvJ22ggDuY2+vUxcFRzb+lxSJd9H4i826l1\n"
			"aDmuriUweBBRHDr9ycV3YpX4rcOl45887QJAQ1UxjBa6qgj2arXZQRgAMxrgWvE9\n"
			"BHc51ZSoK9Fej8+RthyMCeh5nBCHVmwapC6nVCXzpgWE6Mcj78m4UOpuoQJAPV2c\n"
			"jKAiecbMCtB4KQA3FgtQ/nE2zcw/cUfyJs6mnoUOMnE26eIpuLyj/QqMLAUd6d/C\n"
			"omValkcfaSs+wyzEkQJBANpYQPve57avZiw9U20L77VXq0aM4+Dcy1qPmxDI9vss\n"
			"SnaQRfzJBhk+LkyLt1CiSysrp9RcsxHN0zvXl98wCNw=\n"
			"-----END RSA PRIVATE KEY-----\n"
		;
	}

	virtual void TearDown() {
		std::remove(certfname());
		std::remove(keyfname());
	}
};

TEST_F(SSLTest, InitDummy) {
	try {
		monitor::SSLContext ctx(".", ".");
		FAIL() << "Constructor must fail on dummy settings";
	} catch (monitor::SSLContext::Error& e) {
		EXPECT_EQ(std::string("Private key verification failed"), std::string(e.what()));
	}
}

TEST_F(SSLTest, Init) {
	try {
		monitor::SSLContext ctx(certfname(), keyfname());
	} catch (monitor::SSLContext::Error& e) {
		FAIL() << "Constructor must not raise an error";
	}
}

TEST_F(SSLTest, InternetConnection) {
	monitor::SSLContext ctx(certfname(), keyfname());
	monitor::SSLSocket peer(&ctx);

	try {
		peer.connect("gvamail.temenos.com", 443);

		peer.write("GET / HTTP/1.0\n\n");

		std::string buf;
		do {
			std::string chunk = peer.read((size_t)monitor::Socket::FastBufferSize);
			ASSERT_TRUE(chunk.length() > 0) << "Empty buffer after blocking read()";
			buf.append(chunk);
			if (chunk.length() < monitor::Socket::FastBufferSize) break;
		} while(1);

		EXPECT_TRUE(buf.find("HTTP/1") != std::string::npos);

	} catch (monitor::SocketException& e) {
		FAIL() << e.what();
	}
}

class SSLEchoClient: public monitor::ListenSocketClient {
public:
	SSLEchoClient(bool dieOnError = true) : 
		__dieOnError(dieOnError)
	{}
	virtual void Action(monitor::Socket* peer, monitor::ListenSocketThread* thread) {
		std::cerr << "Action: START" << std::endl;
		while (true) {
			try {
				// std::cerr << "Action: before read(1)" << std::endl;
				std::string data = peer->read(1);
				std::cerr << "Action: after read(1) [" << data << "]" << std::endl;
   			peer->write(data);

				if (data == "*") {
					std::cerr << "Get '*'" << std::endl;
					thread->terminate();
					break;
				}
			} catch(monitor::SocketError& e) {
				// This is only for testing. It kills the server thread on any
				// error in the channel. The real server should have another
				// way of terminating.
				std::cerr << "Get '@@@@@@@@@@@@@@@@@@@ [" << e.what() << "]" << std::endl;
				if (__dieOnError)
					thread->terminate();
				break;
			}
		}
		std::cerr << "Action: END" << std::endl;
	}
private:
	bool __dieOnError;
};

#ifdef WIN32
#define popen _popen
#endif

void checkPortListening(int port, bool active) {
	std::string output;

	return;

	std::stringstream fmt;
	fmt << "netstat -an | grep LISTEN | grep :" << port;

	FILE* chld = popen(fmt.str().c_str(), "r");
	
	EXPECT_TRUE(chld != 0) << "Unable to execute " << fmt.str();

	while (!feof(chld)) {
		char buf[1024];
		int sz = fread(reinterpret_cast<void *>(buf), 1, sizeof(buf), chld);
		if (sz > 0)
			output += buf;
	}
	
	fclose(chld);

	if (active)
		ASSERT_NE(0, output.length()) << "No listener on port " << port;
	else
		ASSERT_EQ(0, output.length()) << "Unexpected listener on port " << port << ", " << output;
}

void checkIsPortListen(int port) {
	checkPortListening(port, true);
}

void checkIsPortNotListen(int port) {
	checkPortListening(port, false);
}

TEST_F(SSLTest, ListenSocketSSLClientToNonSSLUnknownProtocol) {
	int port = 40000;

	monitor::SSLContext context(certfname(), keyfname());

	SCOPED_TRACE("");
	checkIsPortNotListen(port);

	monitor::ListenSocketThread* thread = new monitor::ListenSocketThread(
		new monitor::ListenSocket(),
		new SSLEchoClient(false),
		port
	);

	thread->safeStart();		

	SCOPED_TRACE("");
	checkIsPortListen(port);

	try {
		monitor::SSLSocket peer(&context);
		//std::cout << "Connecting..." << std::endl;
		peer.connect("localhost", port);
		//std::cout << "Connected" << std::endl;

		FAIL() << "SSL socket must fail because the listenter is not SSL";
	} catch(monitor::SocketException& e) {
		// std::cerr << "22: " << e.what() << std::endl;
		EXPECT_TRUE(std::string(e.what()).find("SSL_connect()") != std::string::npos) << e.what();
	}

	monitor::Socket stopper;
	stopper.connect("localhost", port);
	stopper.write("*");
	stopper.disconnect();

	std::cerr << "Joining..." << std::endl;
	thread->Join();
	delete thread;

	SCOPED_TRACE("");
	checkIsPortNotListen(port);
}

TEST_F(SSLTest, ListenSocketNonSSLClientToSSL) {
	int port = 40000;

	monitor::SSLContext context(certfname(), keyfname());

	SCOPED_TRACE("");
	checkIsPortNotListen(port);

	monitor::ListenSocketThread* thread = new monitor::ListenSocketThread(
		new monitor::SSLListenSocket(&context),
		new SSLEchoClient(),
		port
	);
	
	thread->safeStart();		

	SCOPED_TRACE("");
	checkIsPortListen(port);

	try {
		monitor::Socket peer;
		peer.connect("localhost", port);

		peer.write("Very long echo test which definitely breaks SSL connection\n");

		// write() must terminate the SSL listener and the listen thread must
		// join us here. If it does not the listener's behavior is wrong. After
		// the listener termination we will check that there is no TCP/IP
		// listener on our port anymore.
		thread->Join();

		SCOPED_TRACE("");
		checkIsPortNotListen(port);

	} catch(monitor::SocketException& e) {
		FAIL() << "Socket should not raise any exception here";
	}

	delete thread;
}

TEST_F(SSLTest, ListenSocket) {
	int port = 40000;

	monitor::SSLContext context(certfname(), keyfname());

	SCOPED_TRACE("");
	checkIsPortNotListen(port);

	monitor::ListenSocketThread* thread = new monitor::ListenSocketThread(
		new monitor::SSLListenSocket(&context),
		new SSLEchoClient(),
		port
	);

	thread->safeStart();

	SCOPED_TRACE("");
	checkIsPortListen(port);

	try {

		monitor::SSLSocket peer(&context);
		peer.connect("localhost", port);

		std::string expected("ECHOTEST");

		peer.write(expected);
		EXPECT_EQ(expected, peer.read(expected.length()));

		peer.write("*");
	} catch(monitor::SocketException& e) {
		FAIL() << e.what();
	}

	thread->Join();
	delete thread;

	SCOPED_TRACE("");
	checkIsPortNotListen(port);
}
