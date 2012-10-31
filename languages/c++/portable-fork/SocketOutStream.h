#ifndef __JCF_SOCKET_OUT_STREAM_H
#define __JCF_SOCKET_OUT_STREAM_H

#include "dllexport.h"

#include "compactjcf.h"

#include "Socket.h"

namespace monitor {

class DllExport SocketOutStream : public jcf::OutStream {
public:
	SocketOutStream(Socket* socket);
	virtual ~SocketOutStream();

	void send(const char *, size_t);
	void flush();

	void setSocket(Socket* socket) { __socket = socket; }
private:
	Socket* __socket;
};

} // namespace monitor

#endif /* __JCF_SOCKET_OUT_STREAM_H */
