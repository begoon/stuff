#include "SocketOutStream.h"

namespace monitor {

SocketOutStream::SocketOutStream(Socket* socket) : 
	__socket(socket) 
{
}

SocketOutStream::~SocketOutStream()
{
}

void SocketOutStream::flush() 
{
}

void SocketOutStream::send(const char *buf, size_t sz) 
{
	__socket->write(buf, sz);
}

} // namespace monitor
