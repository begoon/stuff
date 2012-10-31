#ifndef _PRETIMER_UNIX_H
#define _PRETIMER_UNIX_H

#include <sys/time.h>

#include <stack>

class PreciseTimer {
public:
	typedef long long Counter;

private:
	typedef std::stack< Counter > Counters;

	Counters __counters;
	
public:
	PreciseTimer()
	{
	}

	inline Counter ticks()
	{
		struct timeval tv;
		
		gettimeofday(&tv, NULL);

		return tv.tv_sec * 1000 + tv.tv_usec / 1000;
	}

	inline void mark()
	{
		__counters.push(ticks());
	}

	inline Counter release()
	{
		if( __counters.empty() ) return -1;
		Counter val = ticks() - __counters.top();
		__counters.pop();
		return val;
	}
};

#endif
