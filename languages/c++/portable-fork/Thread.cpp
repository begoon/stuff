#include "Thread.h"

namespace monitor
{

static void ThreadCallback(Thread* who) {
	dynamic_cast<Thread*>(who)->Execute();	
}

#ifdef WIN32

Thread::~Thread() {
	CloseHandle(__handle);
}

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

Thread::~Thread() {
}

extern "C"
typedef void *(*pthread_callback)(void *);

void Thread::Start() {
	pthread_create(
		&__handle, 0, 
		reinterpret_cast<pthread_callback>(ThreadCallback), 
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
