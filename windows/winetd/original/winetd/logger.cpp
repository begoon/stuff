/*
  Event log to file.
  by Alexander S. Pristenski 2007

  Change log:

  29.01.2007  Alexander S. Pristenski - initial creation.
  20.12.2007  Alexander S. Pristenski - ability to completely disable log (AT COMPILE TIME) added.
*/

#include <windows.h>
#include <stdio.h>
#include <stdarg.h>
//#include <varargs.h>
#include <wtypes.h>
#include "logger.h"
#include "cse.h"

const char *filename = "winetd.log";

static CRITICAL_SECTION cs;
static bool debug_mode = false;
static HANDLE hLogFile = INVALID_HANDLE_VALUE;

static _iobuf *StdOut;

void logger::debug_mode_set(bool enable)
{
  debug_mode = enable;
}


/*
  Name: init()

  Description: initialize logger CS to allow multiple thread log.

  Parameters: none.

  Return value: none.

  Revision: 30.01.2007
*/
void logger::init(void)
{
  CSE::MapSEtoCE(); // Must be called before any exceptions are raised
  InitializeCriticalSection(&cs);

  StdOut = stdout;

  // compose full file name
  char fullfname[MAX_PATH+1] = "\0";
  GetModuleFileName(NULL, fullfname, MAX_PATH);
  char *tmp = strrchr(fullfname, '\\');
  if(tmp == NULL) return;
  tmp++; // skip last '\'
  strcpy(tmp, filename);

  logger::debug("Starting logging");
  // try to open existing
  hLogFile = CreateFile(fullfname, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

  // if fails - try to create new
  if(INVALID_HANDLE_VALUE == hLogFile)
  {
    logger::debugfmt("Trying to create log file %s", fullfname);
    hLogFile = CreateFile(fullfname, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  }

  // go to EOF
  if(INVALID_HANDLE_VALUE != hLogFile)
    {
      SetFilePointer(hLogFile, 0, NULL, FILE_END);
    }
    else
    {
      logger::debugfmt("Cannot create log file %s. Giving up :(", fullfname);
    }
}

void logger::close(void)
{
  logger::debug("Shutting down logging...");

  if(INVALID_HANDLE_VALUE != hLogFile)
  {
    CloseHandle(hLogFile);
  }
  DeleteCriticalSection(&cs);  
}

/*
  Name: add()

  Description: add event string to log file.

  Parameters:
        [in] event - various event string.
        [in] filename - log file name in the current dir.

  Return value:
          ON SUCCESS: TRUE
          ON   ERROR: FALSE

  Revision: 20.12.2007
*/

bool logger::add_prefix(const char *log_prefix, const char *event)
{ 
  #if(LOGGER_ENABLED == 1)
  //if ((filename == NULL) && (*filename == 0)) return false;

  EnterCriticalSection(&cs);
  try
    {
      fprintf(StdOut, "%s%s\n", log_prefix, event);

      if( (event != NULL) && (*event != 0)
      // file opened ok
       && (INVALID_HANDLE_VALUE != hLogFile))
      { 
        SYSTEMTIME time;
        GetSystemTime(&time);

        char prefix[512];
        wsprintf(prefix, "\n%.2i.%.2i.%.4i %.2i:%.2i:%.2i - ", time.wDay, time.wMonth, time.wYear, time.wHour, time.wMinute, time.wSecond);

        //printf("%s%s%s", prefix, log_prefix, event);

        DWORD written;

        WriteFile(hLogFile, prefix, lstrlen(prefix), &written, NULL);
        WriteFile(hLogFile, log_prefix, lstrlen(log_prefix), &written, NULL);
        WriteFile(hLogFile, event, lstrlen(event), &written, NULL);           
      }
    }
  catch (CSE se) 
    {
      //silently ignore error
    }
    
  LeaveCriticalSection(&cs);
  #endif // #if(LOGGER_ENABLED == 1)

  return true;
}

bool logger::add(const char *event)
{
  return add_prefix("", event);
}

void logger::addfmt(const char *event, ...)
{
  va_list fmtargs;
  char buffer[4048];

  va_start(fmtargs,event);
  //vsnprintf(buffer,sizeof(buffer)-1,event,fmtargs);
  //vsnprintf_s(buffer,sizeof(buffer)-1, _TRUNCATE, event,fmtargs);
  vsprintf(buffer,event,fmtargs);
  va_end(fmtargs);

  logger::add(buffer);
}

void logger::debug(const char *event)
{
  if (debug_mode)
    add_prefix("DBG - ", event);
}

void logger::debugfmt(const char *event, ...)
{
  va_list fmtargs;
  char buffer[4048];

  va_start(fmtargs,event);
  //vsnprintf(buffer,sizeof(buffer)-1,event,fmtargs);
  //vsnprintf_s(buffer,sizeof(buffer)-1, _TRUNCATE, event,fmtargs);
  vsprintf(buffer,event,fmtargs);
  va_end(fmtargs);

  logger::debug(buffer);
}

void logger::error(const char *event)
{
  add_prefix("ERR - ", event);
}

void logger::errorfmt(const char *event, ...)
{
  va_list fmtargs;
  char buffer[4048];

  va_start(fmtargs,event);
  //vsnprintf(buffer,sizeof(buffer)-1,event,fmtargs);
  //vsnprintf_s(buffer,sizeof(buffer)-1, _TRUNCATE, event,fmtargs);
  vsprintf(buffer,event,fmtargs);
  va_end(fmtargs);

  logger::error(buffer);
}
