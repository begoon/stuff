#ifndef __MONITOR_LISTEN_SOCKET_H
#define __MONITOR_LISTEN_SOCKET_H

#include "dllexport.h"

#include "Thread.h"
#include "Mutex.h"
#include "Socket.h"
#include "SocketException.h"

#include <string>

namespace monitor {

class DllExport ListenSocket
{
public:
	ListenSocket();
	virtual ~ListenSocket();

	void listen(int port, const char* addr = "0.0.0.0");
	virtual Socket* accept();

	bool select();

	virtual void disconnect() throw();

	int handle() const { return __handle; }

protected:
	int do_accept();
	void do_disconnect(int sock) throw();

private:
	int __handle;
	bool __listening;
};

} // monitor

#endif
