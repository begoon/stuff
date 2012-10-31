#ifndef __MONITOR_SYSTEM_MESSAGE_H
#define __MONITOR_SYSTEM_MESSAGE_H

#include "dllexport.h"

#include <string>

namespace monitor {

class DllExport SystemMessage 
{
public:
	static int code();
	static const std::string message(int code);
}; // SystemMessage

} // namespace monitor

#endif /* __MONITOR_SYSTEM_MESSAGE_H */
