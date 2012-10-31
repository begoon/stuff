/*
  Event log to file.
  by Alexander S. Pristenski 2007

  Change log:

  29.01.2007  Alexander S. Pristenski - initial creation.
  20.12.2007  Alexander S. Pristenski - ability to completely disable log (AT COMPILE TIME) added.
*/

#ifndef _LOGGER_H_
#define _LOGGER_H_

#define LOGGER_ENABLED 1

namespace logger
{
  void debug_mode_set(bool enable);

  void init(void);
  void close(void);

  bool add(const char *event);
  void addfmt(const char *event, ... );

  bool add_prefix(const char *log_prefix, const char *event);
  void debug(const char *event);
  void debugfmt(const char *event, ... );
  void error(const char *event);
  void errorfmt(const char *event, ...);
}

#endif
