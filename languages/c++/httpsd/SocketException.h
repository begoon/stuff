#ifndef __MONITOR_SOCKET_EXCEPTION_H
#define __MONITOR_SOCKET_EXCEPTION_H

#include "dllexport.h"

#include "SystemMessage.h"

#include <string>
#include <stdexcept>
#include <sstream>

namespace monitor {

class SocketException: public std::runtime_error {
public:
	SocketException(const std::string& msg) : 
		std::runtime_error(msg)
	{}

	template<typename T>
	std::string itoa(const T& a) {
		std::stringstream fmt;
		fmt << a;
		return fmt.str();
	}
};

class SocketError: public SocketException {
public:
	SocketError(const std::string& msg = "", int code = SystemMessage::code()) :
		__code(code),
		SocketException(msg + ", " + itoa<int>(code) + " (" + SystemMessage::message(code) + ")")
	{}
private:
	int __code;
};

} // namespace monitor

#endif
