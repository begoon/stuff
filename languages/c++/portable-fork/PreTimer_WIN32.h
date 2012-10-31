#ifndef _PRETIMER_WIN32_H
#define _PRETIMER_WIN32_H

#include <stack>

#include <windows.h>

class PreciseTimer {
public:
	typedef LONGLONG Counter;

private:
	LARGE_INTEGER __freq;

	typedef std::stack< Counter > Counters;

	Counters __counters;
	
public:
	PreciseTimer()
	{
		if( !QueryPerformanceFrequency( &__freq ) )
			__freq.QuadPart = 0;
	}

	Counter ticks()
	{
		LARGE_INTEGER current;

		if( __freq.QuadPart == 0 || !QueryPerformanceCounter( &current ) ) return 0;
		return current.QuadPart / (__freq.QuadPart / 1000);
	}

	void mark()
	{
		__counters.push(ticks());
	}

	Counter release()
	{
		if( __counters.empty() ) return -1;
		Counter val = ticks() - __counters.top();
		__counters.pop();
		return val;
	}
};

#endif
