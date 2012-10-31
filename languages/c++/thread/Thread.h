#ifndef _MONITOR_THREAD_H
#define _MONITOR_THREAD_H

#include "dllexport.h"

namespace monitor
{

#ifdef WIN32
#include <windows.h>
typedef HANDLE ThreadType;
#else
#include <pthread.h>
#include <signal.h>
typedef pthread_t ThreadType;
#endif

class DllExport Thread
{
public:
	void Start();
	virtual void Execute() = 0;

	void Join();
	void Kill(int);
private:
	ThreadType __handle;
};

static void ThreadCallback(Thread* who) {
	dynamic_cast<Thread*>(who)->Execute();	
}

#ifdef WIN32

void Thread::Start() {
	__handle = CreateThread(
		0, 0, 
		reinterpret_cast<LPTHREAD_START_ROUTINE>(ThreadCallback), this,
		0, 0
	);
}

void Thread::Join() {
	WaitForSingleObject(__handle,  INFINITE);
}

void Thread::Kill(int) {
	TerminateThread(__handle, 0);
}

#else

void Thread::Start() {
	pthread_create(
		&__handle, 0, 
		reinterpret_cast<void *(*)(void *)>(ThreadCallback), 
		this
	);
}

void Thread::Join() {
	pthread_join(__handle, 0);
}

void Thread::Kill(int sig) {
	pthread_kill(__handle, sig);
}

#endif

} // monitor

#endif
