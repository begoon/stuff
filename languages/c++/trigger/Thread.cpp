#include "Thread.h"

namespace monitor
{

static void ThreadCallback(Thread* who) {
#ifndef WIN32
	int old_thread_type;
	pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, &old_thread_type);
#endif
	who->Execute();	
}

#ifdef WIN32

Thread::Thread() :
	__handle(0)
{}

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

void Thread::Kill() {
	TerminateThread(__handle, 0);
}

#else

Thread::Thread() :
	__handle(0) 
{}

Thread::~Thread() {
}

extern "C"
typedef void *(*pthread_callback)(void *);

void Thread::Start() {
#ifdef AIX
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN + 1024*1024);
	pthread_create(
		&__handle, &attr, 
		reinterpret_cast<pthread_callback>(ThreadCallback), 
		this
	);
	pthread_attr_destroy(&attr);
#else
        pthread_create(
                &__handle, 0,
                reinterpret_cast<pthread_callback>(ThreadCallback),
                this
        );
#endif
}

void Thread::Join() {
	pthread_join(__handle, 0);
}

void Thread::Kill() {
	pthread_cancel(__handle);
}

#endif

} // monitor
