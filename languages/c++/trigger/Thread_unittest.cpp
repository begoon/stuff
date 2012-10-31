#include <gtest/gtest.h>

#include "Thread.h"
#include "PreTimer.h"

class SimpleThread: public monitor::Thread {
public:
	SimpleThread() :
		__done(false)
	{}

	virtual void Execute() {
		__done = true;
	}

	bool done() const { return __done; }
private:
	bool __done;
};

TEST(ThreadTest, RunningInParallel) {
	SimpleThread thread;
	EXPECT_FALSE(thread.done());
	thread.Start();
	thread.Join();
	EXPECT_TRUE(thread.done());
}

class GreedyThread: public monitor::Thread {
public:
	virtual void Execute() {
		while (true) { 
			monitor::PreciseTimer::sleepMs(1);
		}
	}
};

#ifdef HPUX
#define KillThread DISABLED_KillThread
#endif

TEST(ThreadTest, KillThread) {
	GreedyThread thread;
	thread.Start();
	thread.Kill();

	// If Kill() doesn't work we'll never join and the test will hang down.
	thread.Join();
}
