#include "gtest/gtest.h"

#include <typeinfo>
#include <iostream>
#include <exception>
#include <string>

#include <cstring>

#include "SSLContext.h"
#include "Socket.h"

std::string testhost = "localhost";

std::string EchoHost = "?";
int EchoPort = 0;

std::string GeneratorHost = "?";
int GeneratorPort = 0;

std::string HttpHost = "?";
int HttpPort = 0;

std::string SSLHttpHost = "?";
int SSLHttpPort = 0;

int main(int argc, char* _argv[])
{
	char** argv = new char*[argc + 1];
	std::copy(_argv, _argv + argc, argv);

	argv[argc++] = "--gtest_print_time";

	for (int i = 1; i < argc; i++) {
		if (i < argc - 1 && !std::strcmp("-h", argv[i]))
			testhost = argv[i+1];
	}

	std::cout << "Test host: " << testhost << std::endl;

	try {
		monitor::SSLContext::bootstrap();
		monitor::Socket::bootstrap();

		testing::InitGoogleTest(&argc, argv);
		return RUN_ALL_TESTS();
	} catch (std::exception& e) {
		std::cerr << "Unhandled exception: " << typeid(e).name() << ", " << e.what() << std::endl;
	} catch (...) {
		std::cerr << "Unhandled exception" << std::endl;
	}
}
