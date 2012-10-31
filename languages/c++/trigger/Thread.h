#ifndef _MONITOR_THREAD_H
#define _MONITOR_THREAD_H

#include "dllexport.h"

#ifdef WIN32
#include <windows.h>
#else
#include <pthread.h>
#include <signal.h>
#endif

namespace monitor
{

#ifdef WIN32
typedef HANDLE ThreadType;
#else
typedef pthread_t ThreadType;
#endif

class DllExport Thread
{
public:
	Thread();
	virtual ~Thread();

	void Start();
	virtual void Execute() = 0;

	void Join();
	void Kill();
private:
	ThreadType __handle;
};

} // monitor

#endif
