#ifndef __MONITOR_SOCKET_H
#define __MONITOR_SOCKET_H

#include "dllexport.h"

#include "SystemMessage.h"
#include "SocketException.h"

#include <string>
#include <stdexcept>
#include <iostream>

namespace monitor {

class DllExport Socket {
public:
	const static size_t FastBufferSize = 1024;

	Socket(bool inheritable = false);
	Socket(int handle, bool inheritable = false);
	virtual ~Socket();

	virtual void connect(const std::string& host, int port);
	virtual void disconnect() throw();

	virtual void shutdown();

	bool select();

	virtual size_t read(char* buf, size_t sz);
	virtual void write(const char* buf, size_t sz);

	virtual void write(const std::string& val) { 
		write(val.c_str(), val.length()); 
	}

	virtual std::string read(size_t sz);

	virtual size_t avail();

	virtual bool connected() const { return __connected; }

	int handle() const { return __handle; }

	static void bootstrap();

	class ConnectFailure: public SocketError {
	public:
		ConnectFailure(const std::string& host, int port, int code = SystemMessage::code());
	};

private:
	bool __inheritable;
	bool __connected;
	int __handle;
};

} // namespace monitor

#endif /* __MONITOR_SOCKET_H */
