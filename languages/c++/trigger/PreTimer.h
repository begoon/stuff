#ifndef _TAFC_BASE_PRETIMER_H
#define _TAFC_BASE_PRETIMER_H

#include <stack>

#ifdef WIN32
#include <windows.h>
#else
#include <sys/time.h>
#include <unistd.h>
#endif

namespace monitor {

class PreciseTimer {
public:
#ifdef WIN32
	typedef LONGLONG Counter;
#else
	typedef long long Counter;
#endif
	PreciseTimer();

	Counter millisec();

	void mark();
	Counter release();

	static void sleepMs(int ms);
private:
	typedef std::stack< Counter > Counters;

	Counters __counters;

#ifdef WIN32
	LARGE_INTEGER __freq;
#endif
};

} // monitor

#endif // _TAFC_BASE_PRETIMER_H
