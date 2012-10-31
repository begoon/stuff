#include "ListenSocketThread.h"
#include "SocketException.h"
#include "Delay.h"

#include <iostream>

namespace monitor {

ListenSocketThread::ListenSocketThread(
	monitor::ListenSocket* listener, 
	monitor::ListenSocketClient* client,                       
	int port, const char* addr
) :
	__listener(listener),
	__client(client),
	__port(port), __addr(addr),
	__ready(false),
	__terminated(false)
{}

void ListenSocketThread::Execute() {
	try {
		__listener->listen(__port, __addr.c_str());

		__ready = true;

		//std::cerr << "Thread after listen" << std::endl;
		while (!terminated()) {
			monitor::Socket* peer = __listener->accept();
			//std::cerr << "before Action()" << std::endl;
			__client->Action(peer, this);
			//std::cerr << "after Action()" << std::endl;
			delete peer;
		}

	} catch(SocketException& e) {
		// We should log e.what()
	} catch(std::exception& e) {
		// We should log e.what()
	} catch(...) {
		// We should log it.
	}

	__listener->disconnect();
	delete __listener;
	delete __client;
}

void ListenSocketThread::safeStart() {
	Start();
	do {
		msleep(1);
	} while (!__ready);
}
	
void ListenSocketThread::terminate() {
	__terminateLock.Lock();
	__terminated = true;
	__terminateLock.Unlock();
}

bool ListenSocketThread::terminated() {
	__terminateLock.Lock();
	bool result = __terminated;
	__terminateLock.Unlock();
	return result;
}

} // monitor
