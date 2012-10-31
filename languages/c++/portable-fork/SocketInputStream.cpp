#include "SocketInputStream.h"

#include "compactjcf.h"

namespace monitor {

SocketInputStream::SocketInputStream(Socket* socket) :
	__socket(socket)
{
}

SocketInputStream::~SocketInputStream()
{
}

size_t SocketInputStream::readMax(char *buffer, int bufSize) 
{
	size_t n = (size_t)__socket->read(buffer, bufSize);

	if (n < 0) {
		throw new jcf::NONE;
	}

	if (n != (size_t)bufSize) {
		throw new jcf::LAST;
	}

	return n;
}

} // namespace monitor
