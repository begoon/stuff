#include "cppunit/TestFixture.h"

#include "testcompressor.h"

#include "compressor.h"

void TestCompressor::setUp() 
{
}

void TestCompressor::tearDown() 
{
}

void TestCompressor::testLoopback() 
{
	std::string expected = "This is a test message to be packed";

	GZipCompressor compressor;

	std::string packed = compressor.compress(expected);
	std::string actual = compressor.uncompress(packed);

	CPPUNIT_ASSERT_EQUAL_MESSAGE("Pack/unpack loopback", expected, actual);
}
