#ifndef _MONITOR_LOGGER_H
#define _MONITOR_LOGGER_H

namespace monitor
{

void log_init(const char* filename);

void logger(const char* fmt, ...);

void log_reset();
void log_mark(const char* msg);
void log_release();

void log_shift();
void log_unshift();

} // monitor

#endif
