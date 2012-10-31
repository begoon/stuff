#ifndef _COMPRESSORTEST_H
#define _COMPRESSORTEST_H

#include <cppunit/extensions/HelperMacros.h>

class TestCompressor : public CPPUNIT_NS::TestFixture
{
	CPPUNIT_TEST_SUITE( TestCompressor );

	// Test pack/unpack
	CPPUNIT_TEST( testLoopback );

	CPPUNIT_TEST_SUITE_END();

public:
	void setUp();
	void tearDown();

	void testLoopback();
};

#endif
