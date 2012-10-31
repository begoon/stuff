#ifndef _TAFC_BASE_MUTEX_H
#define _TAFC_BASE_MUTEX_H

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

class Mutex {
public:
	inline Mutex();
	inline ~Mutex();
	
	inline void Acquire();
	inline void Release();
private:
	MutexType __mutex;
	
	Mutex(const Mutex&);
	void operator=(const Mutex&);
};

#ifdef WIN32

Mutex::Mutex()             { InitializeCriticalSection(&__mutex); }
Mutex::~Mutex()            { DeleteCriticalSection(&__mutex); }
void Mutex::Acquire()      { EnterCriticalSection(&__mutex); }
void Mutex::Release()      { LeaveCriticalSection(&__mutex); }

#else // WIN32

Mutex::Mutex()             { pthread_mutex_init(&__mutex, NULL); }
Mutex::~Mutex()            { pthread_mutex_destroy(&__mutex); }
void Mutex::Acquire()      { pthread_mutex_lock(&__mutex); }
void Mutex::Release()      { pthread_mutex_unlock(&__mutex); }

#endif // WIN32

class AutoLock {
public:
	AutoLock(Mutex& lock) : __lock(lock) {
		__lock.Acquire();
	}

	~AutoLock() {
		__lock.Release();
	}
private:
	AutoLock(const AutoLock&);
	void operator=(const AutoLock&);

	Mutex& __lock;
};

} // namespace monitor

#endif
