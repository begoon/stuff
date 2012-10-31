#include <cppunit/ui/text/TestRunner.h>
#include "testcompressor.h"

int main(int argc, char* *argv)
{
	CppUnit::TextUi::TestRunner runner;
	runner.addTest(TestCompressor::suite());
	runner.run();
	return 0;
}
