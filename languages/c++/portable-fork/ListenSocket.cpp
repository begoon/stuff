#include "ListenSocket.h"
#include "SystemMessage.h"

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

#include <cstring>

namespace monitor {

ListenSocket::ListenSocket()
{
#ifdef WIN32
	WORD wVersionRequested = MAKEWORD(2, 2);
	WSADATA wsaData;

	__error_code = WSAStartup(wVersionRequested, &wsaData);
#endif
}

ListenSocket::~ListenSocket()
{
#ifdef WIN32
//	WSACleanup();
#endif
}

const std::string ListenSocket::error_message()
{
	return monitor::SystemMessage::message(__error_code);
}

bool ListenSocket::error() 
{ 
	__error_code = monitor::SystemMessage::code(); 
	return __error_code == 0;
}

#ifdef WIN32
#define SOCKOPT_TYPE const char *
#else
#define SOCKOPT_TYPE void *
#endif

bool ListenSocket::listen(int port, const char* listen_addr)
{
	struct sockaddr_in addr;
	int one = 1;

	if ((__listen_socket = ::socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
		return error();
		
#ifdef WIN32
	HANDLE clone;
	if (!DuplicateHandle(
		GetCurrentProcess(), (HANDLE)__listen_socket, 
		GetCurrentProcess(), &clone, 
		0,
		FALSE,
		DUPLICATE_CLOSE_SOURCE | DUPLICATE_SAME_ACCESS
	))
		return error();
	__listen_socket = (int)clone;
#endif

	if (::setsockopt(__listen_socket, SOL_SOCKET, SO_REUSEADDR, (SOCKOPT_TYPE)&one, sizeof(one)) < 0)
		return error();
	
	memset(&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = inet_addr(listen_addr);
	addr.sin_port = htons(port);

	if (::bind(__listen_socket, reinterpret_cast<struct sockaddr *>(&addr), sizeof(addr)) < 0)
		return error();

	if (::listen(__listen_socket, 10) < 0)
		return error();

	return true;
}

Socket* ListenSocket::accept()
{	
	struct sockaddr_in addr;
#if defined(WIN32) || defined(HPUX)
	int addrlen;
#else
	socklen_t addrlen;
#endif
	int sock;

	addrlen = sizeof(addr);
	memset(&addr, 0, sizeof(addr));

	sock = ::accept(__listen_socket, reinterpret_cast<struct sockaddr *>(&addr), &addrlen);
		
	if (sock < 0) {
		error();
		return 0;
	}

	return new Socket(sock, true);
}

bool ListenSocket::select()
{
	fd_set rd;
	FD_ZERO(&rd);
	FD_SET(__listen_socket, &rd);

	int n = ::select(__listen_socket + 1, &rd, 0, 0, 0);

	return (n != -1) && FD_ISSET(__listen_socket, &rd);
}

void ListenSocket::disconnect()
{
#ifdef WIN32
	::shutdown(__listen_socket, SD_BOTH);
	::closesocket(__listen_socket);
#else
	::shutdown(__listen_socket, SHUT_RDWR);
	close(__listen_socket);
#endif
}	

} // monitor
