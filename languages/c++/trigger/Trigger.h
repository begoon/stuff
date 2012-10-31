#ifndef _TAFC_BASE_TRIGGER_H
#define _TAFC_BASE_TRIGGER_H

#ifdef WIN32
	#include <windows.h>
#else
	#include <pthread.h>
#endif

namespace monitor {

class Trigger {
public:
	Trigger();
	~Trigger();
	
	void Signal();
	void Wait();

private:
#ifdef WIN32		
	HANDLE __handle;
#else
	pthread_mutex_t __mutex;
	pthread_cond_t __cv;
#endif

	Trigger(const Trigger&);
	void operator=(const Trigger&);
};

} // namespace monitor

#endif
