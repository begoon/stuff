#ifndef __MONITOR_LISTEN_SOCKET_THREAD_H
#define __MONITOR_LISTEN_SOCKET_THREAD_H

#include "dllexport.h"

#include "Thread.h"
#include "Mutex.h"
#include "Socket.h"
#include "ListenSocket.h"

#include <string>

namespace monitor {

class ListenSocketThread;

class DllExport ListenSocketClient {
public:
	virtual void Action(Socket* peer, ListenSocketThread* thread ) = 0;
};

class DllExport ListenSocketThread: public Thread {
public:
	ListenSocketThread(
		monitor::ListenSocket* listener, 
		monitor::ListenSocketClient* client,
		int port, const char* addr = "0.0.0.0"
	);

	virtual void Execute();

	void safeStart();
	
	bool ready() const { return __ready; }

	void terminate();
	bool terminated();

private:
	monitor::ListenSocket* __listener;
	int __port;
	std::string __addr;

	ListenSocketClient* __client;

	volatile bool __ready;

	volatile bool __terminated;
	Mutex __terminateLock;
};

} // monitor

#endif
