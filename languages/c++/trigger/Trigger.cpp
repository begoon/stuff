#include "Trigger.h"

namespace monitor {

#ifdef WIN32

Trigger::Trigger() { 
	__handle = CreateEvent( 
		NULL,               // default security attributes
		TRUE,               // manual-reset event
		FALSE,              // initial state is nonsignaled
		TEXT("Trigger")     // object name
	); 
}

Trigger::~Trigger() { 
	CloseHandle(__handle);
}

void Trigger::Signal() {
	SetEvent(__handle);
}

void Trigger::Wait() {
	WaitForSingleObject( 
		__handle,      // event handle
		INFINITE       // indefinite wait
	);
	ResetEvent(__handle);
}

#else // WIN32

Trigger::Trigger() { 
	pthread_mutex_init(&__mutex, NULL); 
	pthread_cond_init(&__cv, NULL); 
}

Trigger::~Trigger() { 
	pthread_cond_destroy(&__cv); 
	pthread_mutex_destroy(&__mutex); 
}

void Trigger::Signal() {
	pthread_mutex_lock(&__mutex);
	pthread_cond_signal(&__cv);
	pthread_mutex_unlock(&__mutex);
}

void Trigger::Wait() {
	pthread_mutex_lock(&__mutex);
	pthread_cond_wait(&__cv, &__mutex);
	pthread_mutex_unlock(&__mutex);
}

#endif // WIN32

} // namespace monitor
