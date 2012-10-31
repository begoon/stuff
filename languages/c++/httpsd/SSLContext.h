#ifndef __MONITOR_SSL_CONTEXT_H
#define __MONITOR_SSL_CONTEXT_H

#include "dllexport.h"

#include <string>
#include <stdexcept>

#include <openssl/ssl.h>
#include <openssl/err.h>

namespace monitor {

class DllExport SSLContext {
public:
	SSLContext(const char* certfname, const char* keyfname);
	~SSLContext();

	class Error: public std::runtime_error {
	public:
		Error(const std::string& msg) : std::runtime_error(msg) {}
	};

	static void bootstrap();

	SSL_CTX* ctx() { return __ctx; }

private:
	SSL_CTX *__ctx;
};

} // namespace monitor

#endif
