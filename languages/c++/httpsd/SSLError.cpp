#include "SSLError.h"
#include "SSLContext.h"

#include <sstream>

namespace monitor {

SSLError::SSLError(int code, const std::string& msg) :
	SocketError(msg + ", " + formatSSLError(code))
{}

std::string SSLError::formatSSLError(int code) {
	int e = ERR_get_error();
	char msg[1024];
	ERR_error_string_n(e, msg, sizeof(msg));
	char* p = "?";
	switch (code) {
		case SSL_ERROR_NONE:             p = "SSL_ERROR_NONE"; break;
		case SSL_ERROR_ZERO_RETURN:      p = "SSL_ERROR_ZERO_RETURN"; break;
		case SSL_ERROR_WANT_READ:        p = "SSL_ERROR_WANT_READ"; break;
		case SSL_ERROR_WANT_WRITE:       p = "SSL_ERROR_WANT_WRITE"; break;
		case SSL_ERROR_WANT_CONNECT:     p = "SSL_ERROR_WANT_CONNECT"; break;
		case SSL_ERROR_WANT_ACCEPT:      p = "SSL_ERROR_WANT_ACCEPT"; break;
		case SSL_ERROR_WANT_X509_LOOKUP: p = "SSL_ERROR_WANT_X509_LOOKUP"; break;
		case SSL_ERROR_SYSCALL:          p = "SSL_ERROR_SYSCALL"; break;
		case SSL_ERROR_SSL:              p = "SSL_ERROR_SSL"; break;
	}
	std::stringstream fmt;
	fmt << code << " (" << p << "), " << msg;
	return std::string(fmt.str());
}

} // monitor
