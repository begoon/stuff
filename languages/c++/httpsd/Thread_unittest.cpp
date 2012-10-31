#include <gtest/gtest.h>

#include "Thread.h"

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

TEST(ThreadTest, Simple) {
	SimpleThread thread;
	EXPECT_FALSE(thread.done());
	thread.Start();
	thread.Join();
	EXPECT_TRUE(thread.done());
}
