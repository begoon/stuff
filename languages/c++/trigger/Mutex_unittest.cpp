#include <gtest/gtest.h>

#include "Mutex.h"
#include "Thread.h"
#include "PreTimer.h"

class A: public monitor::Thread {
public:
	A(volatile int& flag, monitor::Mutex& mutex) :
		__flag(flag), __mutex(mutex)
	{}

	virtual void Execute() {
		__mutex.Acquire();
		__flag = 1;
		__mutex.Release();
	}

private:
	volatile int& __flag;
	monitor::Mutex& __mutex;
};

TEST(Mutex, Generic) {
	volatile int flag = 0;

	monitor::Mutex mutex;
	mutex.Acquire();

	A a(flag, mutex);
	a.Start();
	monitor::PreciseTimer::sleepMs(100);

	EXPECT_EQ(0, flag);

	mutex.Release();

	a.Join();
	EXPECT_EQ(1, flag);
}

class T: public monitor::Thread {
public:
	T(volatile int& flag, monitor::Mutex& mutex, int timeout, int val) :
		__flag(flag), __mutex(mutex), __timeout(timeout), __val(val)
	{}

	virtual void Execute() {
		monitor::AutoLock locker(__mutex);
		__flag = __val;
		monitor::PreciseTimer::sleepMs(__timeout);
	}
private:
	volatile int& __flag;
	monitor::Mutex& __mutex;
	int __timeout;
	int __val;
};

TEST(AutoLock, ConcurrentCalls) {
	volatile int flag = 0;

	monitor::Mutex mutex;

	T a(flag, mutex, 100, 1);
	T b(flag, mutex, 0, 0);

	a.Start();
	while (!flag);

	b.Start();
	monitor::PreciseTimer::sleepMs(50);

	EXPECT_EQ(1, flag);

	a.Join();
	b.Join();

	EXPECT_EQ(0, flag);
}
