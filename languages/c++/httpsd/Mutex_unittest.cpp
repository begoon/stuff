#include <gtest/gtest.h>

#include "Mutex.h"
#include "Thread.h"
#include "Delay.h"

class A: public monitor::Thread {
public:
	A(volatile int& flag, monitor::Mutex& mutex) :
		__flag(flag), __mutex(mutex)
	{}

	virtual void Execute() {
		__mutex.Lock();
		__flag = 1;
		__mutex.Unlock();
	}

private:
	volatile int& __flag;
	monitor::Mutex& __mutex;
};

TEST(MutexText, Simple) {
	volatile int flag = 0;

	monitor::Mutex mutex;
	mutex.Lock();

	A a(flag, mutex);
	a.Start();
	msleep(100);

	EXPECT_EQ(0, flag);

	mutex.Unlock();

	a.Join();
	EXPECT_EQ(1, flag);
}
