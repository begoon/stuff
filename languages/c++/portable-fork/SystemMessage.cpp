#include "SystemMessage.h"

#ifdef WIN32
#include <windows.h>
#else
#include <string.h>
#include <unistd.h>
#include <errno.h>
#endif

namespace monitor {

int SystemMessage::code()
{
#ifdef WIN32
	return GetLastError();
#else
	return errno;
#endif
}

#ifndef WIN32
#ifndef NO_STRERROR_R
extern "C" int sys_nerr;
extern "C" char* sys_errlist[];
#endif
#endif

const std::string SystemMessage::message(int code)
{
	char msg[1024];

#ifdef WIN32

	char* p = msg;

	FormatMessage(
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		code,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		msg,
		sizeof(msg) - 1,
		NULL
	);

	p = msg + strlen(msg);
	
	/* trim bloody CRLFs and '.' */
	for(p = msg + strlen(msg) - 1; p >= msg && (*p == '\n' || *p == '\r' || *p == '.');) 
		*p-- = 0;

#elif NO_STRERROR_R

	if (code < 0 || code >= sys_nerr)
		return "?";

	strncpy(msg, sys_errlist[code], sizeof(msg) - 1);

	// If an error message is longer than sizeof(msg)-1, '\0' will not be
	// copied that's why we put it manually.
	msg[sizeof(msg) - 1] = 0;

#else

	if (strerror_r(code, msg, sizeof(msg) - 1) < 0)
		return "?";

#endif

	return msg;
}

} // namespace monitor
