#ifndef __MONITOR_LISTEN_SOCKET_H
#define __MONITOR_LISTEN_SOCKET_H

#include "dllexport.h"

#include "Socket.h"

#include <string>

namespace monitor {

class ListenSocket
{
public:
	ListenSocket();
	~ListenSocket();

	bool listen(int port, const char* addr = "0.0.0.0");
	Socket* accept();

	bool select();

	void disconnect();

	int error_code() { return __error_code; }
	const std::string error_message();
	bool error();

private:
	int __listen_socket;

	int __error_code;
};

} // monitor

#endif
