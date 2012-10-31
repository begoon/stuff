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

#include "ListenSocket.h"
#include "SystemMessage.h"
#include "Delay.h"
#include "SocketException.h"

#include <cstring>

namespace monitor {

ListenSocket::ListenSocket() :
	__listening(false)
{}

ListenSocket::~ListenSocket() {
	if (__listening)
		disconnect();		
}

#ifdef WIN32
#define SOCKOPT_TYPE const char *
#else
#define SOCKOPT_TYPE void *
#endif

void ListenSocket::listen(int port, const char* listen_addr)
{
	struct sockaddr_in addr;
	int one = 1;

	if ((__handle = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
		throw SocketError("ListenSocket::listen::socket()");
		
#ifdef WIN32
	HANDLE clone;
	if (!DuplicateHandle(
		GetCurrentProcess(), (HANDLE)__handle, 
		GetCurrentProcess(), &clone, 
		0,
		FALSE,
		DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS
	))
		throw SocketError("ListenSocket::listen::DuplicateHandle()");
	__handle = (int)clone;
#endif

	if (::setsockopt(__handle, SOL_SOCKET, SO_REUSEADDR, (SOCKOPT_TYPE)&one, sizeof(one)) < 0)
		throw SocketError("ListenSocket::listen::setsockopt()");
	
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = inet_addr(listen_addr);
	addr.sin_port = htons(port);

	if (::bind(__handle, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)) < 0)
		throw SocketError("ListenSocket::listen::bind()");

	if (::listen(__handle, 10) < 0)
		throw SocketError("ListenSocket::listen::listen()");

	__listening = true;
}

int ListenSocket::do_accept() {
	struct sockaddr_in addr;
#if defined(WIN32)
	int addrlen;
#else
	socklen_t addrlen;
#endif

	addrlen = sizeof(addr);
	memset(&addr, 0, sizeof(addr));

	int sock = ::accept(__handle, reinterpret_cast<struct sockaddr *>(&addr), &addrlen);

	if (sock < 0)
		throw SocketError("ListenSocket::do_accept::accept()");

	return sock;
}

Socket* ListenSocket::accept()
{	
	return new Socket(do_accept(), true);
}

bool ListenSocket::select()
{
	fd_set rd;
	FD_ZERO(&rd);
	FD_SET(__handle, &rd);

	fd_set er;
	FD_ZERO(&er);
	FD_SET(__handle, &er);

	int n = ::select(__handle + 1, &rd, 0, &er, 0);

	if (n < 0)
		throw SocketError("ListenSocket::select::select()");

	if (FD_ISSET(__handle, &er))
		throw SocketError("ListenSocket::select::FD_ISSET()");

	return FD_ISSET(__handle, &rd);
}

void ListenSocket::do_disconnect(int sock) throw()
{
#ifdef WIN32
	::shutdown(sock, SD_BOTH);
	::closesocket(sock);
#else
	::shutdown(sock, SHUT_RDWR);
	close(sock);
#endif
	__listening = false;
}	

void ListenSocket::disconnect() throw()
{
	do_disconnect(__handle);
}	

} // monitor
