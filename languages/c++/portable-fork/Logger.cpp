#include <fstream>
#include <iomanip>

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>
#include <ctime>

#include "Logger.h"

#ifdef WIN32
#include "PreTimer_WIN32.h"
#else
#include "PreTimer_UNIX.h"
#endif

/*
 * All this stuff is very C-focus only because of a simplicity of using.
 */

namespace monitor {

static std::string log_filename;
static std::string log_offset;

static PreciseTimer log_timer;

void log_init(const char* filename)
{
	log_filename = filename;
}

void logger(const char* fmt, ...)
{
	char buf[ 10240 ];

	std::va_list ap;
	va_start(ap, fmt);
	std::vsprintf(buf, fmt, ap);

	std::time_t stamp = std::time(NULL);
	struct std::tm* lt = std::localtime(&stamp);

	char log[ 10240 ];

	std::sprintf(log,
		"%04d/%02d/%02d %02d:%02d:%02d | %s%s",
		lt->tm_year + 1900, lt->tm_mon + 1, lt->tm_mday, lt->tm_hour, lt->tm_min, lt->tm_sec,
		log_offset.c_str(), buf
	);

	std::ofstream os(log_filename.c_str(), std::ios_base::app);
	os << log << std::endl;
}

void log_reset()
{
	log_offset.clear();
}

void log_mark(const char* msg)
{
	logger(msg);
	log_timer.mark();
	log_shift();
}

void log_release()
{
	log_unshift();
	logger("= %5.3f sec.", (float)log_timer.release()/1000);
}

void log_shift()
{
	log_offset.resize(log_offset.length() + 3, ' ');
}

void log_unshift()
{
	if(log_offset.length() <= 0) return;

	log_offset.resize(log_offset.length() - 3);
}

} // monitor
