#include <gtest/gtest.h>

#include "Trigger.h"
#include "Thread.h"
#include "PreTimer.h"

#include <iostream>

class TriggerThread: public monitor::Thread {
public:
	TriggerThread(volatile int& flag, monitor::Trigger& cv) :
		__flag(flag), __cv(cv)
	{}

	virtual void Execute() {
		__cv.Wait();
		__flag = 1;
		__cv.Wait();
		__flag = 2;
		__cv.Wait();
		__flag = 3;
	}

private:
	volatile int& __flag;
	monitor::Trigger& __cv;
};

TEST(Trigger, Generic) {
	volatile int flag = 0;
	monitor::Trigger cv;

	TriggerThread a(flag, cv);
	a.Start();
	monitor::PreciseTimer::sleepMs(10);
	EXPECT_EQ(0, (int)flag);
   
   cv.Signal();
	monitor::PreciseTimer::sleepMs(10);
	EXPECT_EQ(1, (int)flag);

   cv.Signal();
	monitor::PreciseTimer::sleepMs(10);
	EXPECT_EQ(2, (int)flag);

   cv.Signal();
	a.Join();
	EXPECT_EQ(3, (int)flag);
}
