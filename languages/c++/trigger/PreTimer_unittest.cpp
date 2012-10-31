#include <gtest/gtest.h>
#include <cstdlib>

#include "PreTimer.h"

TEST(PreciseTimer, PreciseSystemTimerAvailability) {
	monitor::PreciseTimer timer;
	EXPECT_NE(0, timer.millisec()) 
		<< "Timer returns 0, the precise system timer is not available.";
}

TEST(PreciseTimer, MeasurementAccuracy) {
	if (std::getenv("TAFC_UNDER_VM")) return;

	const int delay_ms = 100;
	const int allowed_delta_ms = 10;

	monitor::PreciseTimer timer;

	timer.mark();
	timer.sleepMs(delay_ms);
	int delta = delay_ms - static_cast<int>(timer.release());

	EXPECT_TRUE(delta <= allowed_delta_ms) 
		<< "Delta (" << delta << ") > than " << allowed_delta_ms;
}

TEST(PreciseTimer, Queue) {
	if (std::getenv("TAFC_UNDER_VM")) return;

	monitor::PreciseTimer timer;
	timer.mark();
	timer.mark();
	monitor::PreciseTimer::Counter a = timer.release();
	monitor::PreciseTimer::sleepMs(100);
	EXPECT_LT(a, timer.release());
}

TEST(PreciseTimer, EmptyQueue) {
	monitor::PreciseTimer timer;
	EXPECT_EQ(-1, timer.release());
}
