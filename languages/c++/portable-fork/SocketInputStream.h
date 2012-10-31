#ifndef __JCF_SOCKET_INPUT_STREAM_H
#define __JCF_SOCKET_INPUT_STREAM_H

#include "compactjcf.h"

#include "Socket.h"

namespace monitor {

class DllExport SocketInputStream : public jcf::InputStream {
public:
	SocketInputStream(Socket* socket);
	virtual ~SocketInputStream();

	virtual size_t readMax(char *buffer, int bufSize);

	void setSocket(Socket* socket) { __socket = socket; }
private:
	Socket* __socket;
};

} // namespace monitor

#endif /* __JCF_SOCKET_INPUT_STREAM_H */
