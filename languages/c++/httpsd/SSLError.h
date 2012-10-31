#ifndef __MONITOR_SSL_ERROR_H
#define __MONITOR_SSL_ERROR_H

#include "dllexport.h"

#include "SocketException.h"

#include <string>

namespace monitor {

class SSLError: public SocketError {
public:
	SSLError(int code, const std::string& msg = "");
private:
	std::string formatSSLError(int code);
};

} // monitor

#endif
