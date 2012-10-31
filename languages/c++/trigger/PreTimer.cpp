#include "PreTimer.h"

namespace monitor {

void PreciseTimer::mark() {
	__counters.push(millisec());
}

PreciseTimer::Counter PreciseTimer::release()	{
	if( __counters.empty() ) return -1;
	Counter val = millisec() - __counters.top();
	__counters.pop();
	return val;
}

#ifdef WIN32

PreciseTimer::PreciseTimer() {
	if (!QueryPerformanceFrequency(&__freq))
		__freq.QuadPart = 0;
}

PreciseTimer::Counter PreciseTimer::millisec() {
	LARGE_INTEGER current;
	if (__freq.QuadPart == 0 || !QueryPerformanceCounter(&current)) 
		return 0;
	return current.QuadPart / (__freq.QuadPart / 1000);
}

void PreciseTimer::sleepMs(int ms) {
	Sleep(ms);
}

#else // WIN32

PreciseTimer::PreciseTimer() {}

PreciseTimer::Counter PreciseTimer::millisec() {
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

void PreciseTimer::sleepMs(int ms) {
	usleep(ms * 1000);
}

#endif // WIN32

} // monitor
