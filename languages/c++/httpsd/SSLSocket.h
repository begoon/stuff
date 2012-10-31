#ifndef __MONITOR_SSL_SOCKET_H
#define __MONITOR_SSL_SOCKET_H

#include "dllexport.h"

#include "Socket.h"
#include "SSLContext.h"

#include <string>

namespace monitor {

class DllExport SSLSocket: public Socket {
public:
	SSLSocket(SSLContext* ctx, bool inheritable = false);
	SSLSocket(SSLContext* ctx, int handle, bool inheritable = false);
	virtual ~SSLSocket();

	virtual void connect(const std::string& host, int port);
	virtual void disconnect() throw();

	virtual void shutdown();

	virtual size_t read(char* buf, size_t sz);
	virtual void write(const char* buf, size_t sz);

	virtual void write(const std::string& val) { 
		Socket::write(val); 
	}

	virtual std::string read(size_t sz) {
		return Socket::read(sz);
	}

	virtual size_t avail();

	virtual bool connected() const { return __connected; }

	SSL* ssl() { return __ssl; }

private:
	SSLContext* __ctx;
	SSL* __ssl;

	bool __connected;
};

} // namespace monitor

#endif
