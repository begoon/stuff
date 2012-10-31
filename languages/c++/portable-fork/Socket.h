#ifndef __MONITOR_SOCKET_H
#define __MONITOR_SOCKET_H

#include "dllexport.h"

#include <string>

namespace monitor {

class DllExport Socket {
public:
	Socket(bool inheritable = false);
	Socket(int handle, bool inheritable = false);
	~Socket();

	bool connect(const char* host, int port);
	void disconnect();
	void shutdown();

	bool select();

	size_t read(char* buf, size_t sz);
	bool write(const char* buf, size_t sz);

	bool connected() { return __connected; }

	int error_code() { return __error_code; }
	const std::string error_message();

	int handle() const { return __handle; }
private:
	void init();

	bool error();

	bool __inheritable;

	bool __connected;

	int __handle;
	int __error_code;
};

} // namespace monitor

#endif /* __MONITOR_SOCKET_H */
