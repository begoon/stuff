#ifndef _MONITOR_DELAY

#define _MONITOR_DELAY
#ifdef WIN32
#include <direct.h>
#include <windows.h>
#define msleep(x)    Sleep(x)
#else
#include <unistd.h>
#define msleep(x)    usleep((x)*1000)
#include <sys/stat.h>
#endif

#endif
