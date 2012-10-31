#ifndef _MONITOR_MUTEX_H
#define _MONITOR_MUTEX_H

#include "dllexport.h"

#ifdef WIN32
	#define WIN32_LEAN_AND_MEAN
	#define NOGDI
	#include <windows.h>
#else
	#include <stdlib.h>
	#include <pthread.h>
#endif

namespace monitor {

#ifdef WIN32
	typedef CRITICAL_SECTION MutexType;
#else
	typedef pthread_mutex_t MutexType;
#endif

class DllExport Mutex {
public:
	inline Mutex();
	inline ~Mutex();
	
	inline void Lock();
	inline void Unlock();
private:
	MutexType __mutex;
	
	Mutex(const Mutex&);
	void operator=(const Mutex&);
};

#ifdef WIN32

Mutex::Mutex()             { InitializeCriticalSection(&__mutex); }
Mutex::~Mutex()            { DeleteCriticalSection(&__mutex); }
void Mutex::Lock()         { EnterCriticalSection(&__mutex); }
void Mutex::Unlock()       { LeaveCriticalSection(&__mutex); }

#else // WIN32

Mutex::Mutex()             { pthread_mutex_init(&__mutex, NULL); }
Mutex::~Mutex()            { pthread_mutex_destroy(&__mutex); }
void Mutex::Lock()         { pthread_mutex_lock(&__mutex); }
void Mutex::Unlock()       { pthread_mutex_unlock(&__mutex); }

#endif // WIN32

}

#endif
