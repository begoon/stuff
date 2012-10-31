#include "Socket.h"
#include "SystemMessage.h"

#include <sstream>
#include <iostream>

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
#include <sys/ioctl.h>
#ifdef SUN
#include <sys/filio.h>
#endif
#ifdef HPUX
#include <sys/ioctl.h>
#include <stropts.h>
#endif
#endif

namespace monitor {

Socket::ConnectFailure::ConnectFailure(const std::string& host, int port, int code) :
	SocketError(host + ":" + itoa<int>(port))
{}

Socket::Socket(bool inheritable) :
	__inheritable(inheritable),
	__connected(false)
{}

Socket::Socket(int handle, bool inheritable) :
	__handle(handle),
	__inheritable(inheritable),
	__connected(true)
{}

Socket::~Socket()
{
	if (__connected)
		disconnect();
}

void Socket::bootstrap()
{
#ifdef WIN32
	WORD wVersionRequested = MAKEWORD(2, 2);
	WSADATA wsaData;

	int error = WSAStartup(wVersionRequested, &wsaData);
	if (error)
		throw SocketError("Unable to load winsock2 library", error);
#endif
}

void Socket::connect(const std::string& host, int port)
{
	if ((__handle = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
		throw SocketError("Socket::connect::socket()");

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
			throw SocketError("Socket::connect::DuplicateHandle()");
		__handle = (int)clone;
	}
#endif
			
	struct sockaddr_in addr;

	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;

	addr.sin_addr.s_addr = ::inet_addr(host.c_str());

	if (addr.sin_addr.s_addr == -1) {
		struct hostent* resolver = ::gethostbyname(host.c_str());
		if (!resolver) 
			throw ConnectFailure(host, port);
		addr.sin_addr.s_addr = ((struct in_addr*)resolver->h_addr)->s_addr;
	}

	addr.sin_port = htons(port);
	if (::connect(__handle, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)) < 0)
		throw ConnectFailure(host, port);
	
	__connected = true;
}

void Socket::disconnect() throw()
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

	if (n < 0)
		throw SocketError("Socket::select::select()");

	if (FD_ISSET(__handle, &er))
		throw SocketError("Socket::select::FD_ISSET()");

	return FD_ISSET(__handle, &rd);
}

size_t Socket::read(char* buf, size_t count)
{
	size_t offset = 0;

	while (count > 0) {
		int n = ::recv(__handle, buf + offset, (int)count, 0);
		if (n < 0)
			throw SocketError("Socket::read::recv()");
		if (!n) return offset;
		count -= n;
		offset += n;
	}

	return offset;
}

std::string Socket::read(size_t sz) {
	if (sz <= FastBufferSize) {
		char fast[FastBufferSize];
		sz = read(fast, sz);
		return std::string(fast, sz);
	}
	char* slow = new char[sz];
	sz = read(slow, sz);
	std::string result(slow, sz);
	delete[] slow;
	return result;
}

size_t Socket::avail() {
#ifdef WIN32
	unsigned long n;
	if (ioctlsocket(__handle, FIONREAD, &n))
		throw SocketError("Socket::avail::ioctlsocket()");
#else
	int n;
	if (::ioctl(__handle, FIONREAD, &n) < 0)
		throw SocketError("Socket::avail::ioctl()");
#endif
	return static_cast<size_t>(n);
}

void Socket::write(const char* buf, size_t sz)
{
	int n = ::send(__handle, buf, (int)sz, 0);
	if (n != sz) throw SocketError("Socket::write::send()");
}

} // namespace monitor
