#include "SSLSocket.h"
#include "SSLError.h"

namespace monitor {

SSLSocket::SSLSocket(SSLContext* ctx, bool inheritable) :
	__ctx(ctx),
	Socket(inheritable)
{
	__ssl = SSL_new(__ctx->ctx());
}

SSLSocket::SSLSocket(SSLContext* ctx, int handle, bool inheritable) :
	__ctx(ctx),
	Socket(handle, inheritable)
{
	__ssl = SSL_new(__ctx->ctx());
	SSL_set_fd(__ssl, Socket::handle());
}

SSLSocket::~SSLSocket()
{
	if (__connected)
		disconnect();
	SSL_free(__ssl);
}

void SSLSocket::connect(const std::string& host, int port)
{
//	std::cerr << "SSLSocket 1" << std::endl;
	Socket::connect(host, port);

//	std::cerr << "SSLSocket 2" << std::endl;
	SSL_set_fd(__ssl, handle());

//	std::cerr << "SSLSocket 3" << std::endl;
	int err = SSL_connect(__ssl);
//	std::cerr << "SSLSocket 4, " << err << std::endl;
	if (err != 1) {
//		std::cerr << "SSLSocket 5, " << err << std::endl;
		throw SSLError(SSL_get_error(__ssl, err), "SSLSocket::connect::SSL_connect()");
	}
}

void SSLSocket::disconnect() throw()
{
	Socket::disconnect();	
	__connected = false;
}

void SSLSocket::shutdown()
{
	SSL_shutdown(__ssl);
	Socket::shutdown();
}

size_t SSLSocket::read(char* buf, size_t count)
{
	size_t offset = 0;

	while (count > 0) {
		int n = SSL_read(__ssl, buf + offset, (int)count);
		if (n < 0)
			throw SSLError(SSL_get_error(__ssl, n), "SSLSocket::read::SSL_read()");
		if (!n) return offset;
		count -= n;
		offset += n;
	}

	return offset;
}

void SSLSocket::write(const char* buf, size_t sz)
{
	int n = SSL_write(__ssl, buf, (int)sz);
	if (n < 0)
		throw SSLError(SSL_get_error(__ssl, n), "SSLSocket::write::SSL_write()");
}

size_t SSLSocket::avail() {
	int n = SSL_pending(__ssl);
	if (n < 0)
		throw SSLError(SSL_get_error(__ssl, n), "SSLSocket::avail::SSL_pending()");
	return n;
}

} // namespace monitor
