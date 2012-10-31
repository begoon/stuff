#ifndef _MONITOR_THREAD_H
#define _MONITOR_THREAD_H

#include "dllexport.h"

#ifdef WIN32
#include <windows.h>
typedef HANDLE ThreadType;
#else
#include <pthread.h>
#include <signal.h>
typedef pthread_t ThreadType;
#endif

namespace monitor
{

class DllExport Thread
{
public:
	void Start();
	virtual void Execute() = 0;
	virtual ~Thread();

	void Join();
	void Kill(int);
private:
	ThreadType __handle;
};

} // monitor

#endif
