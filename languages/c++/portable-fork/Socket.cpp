#include "Socket.h"
#include "SystemMessage.h"

#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <ctype.h>

#ifdef WIN32
#include <winsock2.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#endif

namespace monitor {

void Socket::init()
{
	__connected = false;
	__error_code = 0;
	__inheritable = false;

#ifdef WIN32
	WORD wVersionRequested = MAKEWORD(2, 2);
	WSADATA wsaData;

	__error_code = WSAStartup(wVersionRequested, &wsaData);
#endif
}

Socket::Socket(bool inheritable) :
	__inheritable(inheritable)
{
	init();
}

Socket::Socket(int handle, bool inheritable)
{
	init();
	__handle = handle;
	__inheritable = inheritable;
	__connected = true;
}

Socket::~Socket()
{
	if (__connected)
		disconnect();
#ifdef WIN32
//	WSACleanup();
#endif
}

bool Socket::connect(const char* host, int port)
{
	struct sockaddr_in addr;
	struct hostent* resolver;

	if ((__handle = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
		return error();

#ifdef WIN32
	if (!__inheritable) {
		HANDLE clone;
		if (!DuplicateHandle(
			GetCurrentProcess(), (HANDLE)__handle, 
			GetCurrentProcess(), &clone, 
			0,
			FALSE,
			DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS
		))
			return error();
		__handle = (int)clone;
	}
#endif
			
	resolver = gethostbyname(host);
	if (!resolver) return error();
	
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = ((struct in_addr*)resolver->h_addr)->s_addr;
	addr.sin_port = htons(port);
	if (::connect(__handle, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)) < 0)
		return error();
	
	__connected = true;

	return true;
}

void Socket::disconnect()
{
	/* make sure that all data in the channel is delivered */
#ifdef WIN32
	::closesocket(__handle);
#else
	close(__handle);
#endif
	__connected = false;
}

void Socket::shutdown()
{
#ifdef WIN32
	::shutdown(__handle, SD_BOTH);
#else
	::shutdown(__handle, SHUT_RDWR);
#endif
}

bool Socket::select()
{
	fd_set rd;
	FD_ZERO(&rd);
	FD_SET(__handle, &rd);

	fd_set er;
	FD_ZERO(&er);
	FD_SET(__handle, &er);

	int n = ::select(__handle + 1, &rd, 0, &er, 0);

	return (n != -1) && FD_ISSET(__handle, &rd) && !FD_ISSET(__handle, &er);
}

size_t Socket::read(char* buf, size_t count)
{
	size_t offset = 0;

	while (count > 0) {
		int n = ::recv(__handle, buf + offset, (int)count, 0);
		if (n < 0) {
			error();
			return n;
		}
		if (!n) return offset;
		count -= n;
		offset += n;
	}

	return offset;
}

bool Socket::write(const char* buf, size_t sz)
{
	int n = ::send(__handle, buf, (int)sz, 0);
	if (n < 0) return error();
	return (size_t)n == sz;
}

bool Socket::error()
{
	__error_code = SystemMessage::code();
	return __error_code == 0;
}

const std::string Socket::error_message()
{
	return SystemMessage::message(__error_code);
}

} // namespace monitor
