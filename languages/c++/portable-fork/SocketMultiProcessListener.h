#ifndef _MONITOR_SOCKET_MULTIPROCESS_LISTENER_H
#define _MONITOR_SOCKET_MULTIPROCESS_LISTENER_H

#ifdef WIN32
#include <windows.h>
#define msleep(x) Sleep(x)
#else
#include <unistd.h>
#define msleep(x) usleep((x)*1000)
#endif

#include <string>

#include "Socket.h"
#include "Thread.h"

class SocketMultiProcessListener {
public:
	SocketMultiProcessListener();

	void child(int argc, char** argv);

	void listen(int port, const char* host = "0.0.0.0");

	void action(monitor::Socket* peer);
	std::string id() const;

	bool ready() const { return __ready; }

	virtual int execute(monitor::Socket* peer) = 0;
	virtual std::string name() const = 0;

private:
	void action(int sock);

	std::string __exe;
	bool __ready;
};

class SocketMultiProcessListenerThread: public monitor::Thread {
public:
	SocketMultiProcessListenerThread(
		SocketMultiProcessListener* listener, 
		int port, const char* addr = "0.0.0.0"
	) :
		__listener(listener),
		__port(port), __addr(addr)
	{}

	virtual void Execute() {
		__listener->listen(__port, __addr.c_str());
	}

	void safeStart() {
		Start();
		do {
			msleep(1);
		} while (!__listener->ready());
	}
	
	bool ready() { return __listener->ready(); }
private:
	SocketMultiProcessListener* __listener;
	int __port;
	std::string __addr;
};


#endif
