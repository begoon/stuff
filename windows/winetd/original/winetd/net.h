/*
	NET - just handy wrapper for Winsock32 and WinInet.
	by Alexander S. Pristenski 2007

	Change log:

	16.01.2007	Alexander S. Pristenski - initial creation.
*/

#ifndef _NET_H_
#define _NET_H_

//#define LOG_TO_FILE

namespace net
{
	extern enum t_last_err {OK, ERR, WAIT} last_err;

	bool	init(void);
	bool	enum_ips(char *buf, unsigned long buf_size);
	bool	get_ie_proxy_settings(char *buf, unsigned long buf_size);
	bool	connect_multi(SOCKET *s, char *server);
	bool	connect(SOCKET *s, char *server);
	bool	bind_and_listen(SOCKET *s, const char *iface, unsigned short port);
	SOCKET*	accept(SOCKET *s);
	char*	getpeername(SOCKET *s);
	bool	connect_via_http_proxy(SOCKET *s, char *proxy, char *server);
	bool	keepalive_on(SOCKET *s, u_long time, u_long interval);
	long	recv(SOCKET *s, char *buf, unsigned long buf_size);
	long	recvall(SOCKET *s, char *buf, unsigned long buf_size);
	bool	sendall(SOCKET *s, const char *buf, unsigned long left);
	void	disconnect(SOCKET *s);

	unsigned long __stdcall hget(void *param);
	unsigned long __stdcall fput(void *param);
	unsigned long __stdcall fdel(void *param);
}

#endif