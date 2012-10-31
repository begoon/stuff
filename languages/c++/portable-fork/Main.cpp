#include "gtest/gtest.h"

#include "SocketMultiProcessListener_unittest.h"

#include "Logger.h"

#include <sstream>

#ifdef WIN32
#define getpid() GetProcessId(GetCurrentProcess())
#endif

int main(int argc, char* argv[])
{
	std::stringstream fmt;

	fmt << "log-" << getpid();

	monitor::log_init(fmt.str().c_str());
	monitor::logger("start");

	for (int i = 0; i < argc; i++)
		monitor::logger("%d: %s", i, argv[i]);

	SimpleClient().child(argc, argv);
	SequenceClient().child(argc, argv);
	EchoClient().child(argc, argv);

	testing::InitGoogleTest(&argc, argv);

	return RUN_ALL_TESTS();
}
