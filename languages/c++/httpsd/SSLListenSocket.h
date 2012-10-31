#ifndef __MONITOR_SSL_LISTEN_SOCKET_H
#define __MONITOR_SSL_LISTEN_SOCKET_H

#include "dllexport.h"

#include "SSLContext.h"
#include "ListenSocket.h"

#include <string>

namespace monitor {

class SSLListenSocket: public ListenSocket
{
public:
	SSLListenSocket(SSLContext* ctx);
	~SSLListenSocket();

	virtual Socket* accept();

private:
	SSLContext* __ctx;
};

} // monitor

#endif
