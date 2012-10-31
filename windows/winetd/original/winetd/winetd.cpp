/*
  Winetd main module.
  by Alexander S. Pristenski 2007

  Change log:

  29.01.2007  Alexander S. Pristenski - initial creation.
  01.09.2007  Alexander S. Pristenski - feature: setup of interface to listen on added.
*/

#pragma comment(linker,"/MERGE:.rdata=.text")
#pragma comment(linker,"/FILEALIGN:512 /SECTION:.text,EWRX /IGNORE:4078")

#include <windows.h>
#include <process.h>
#include "net.h"
#include "logger.h"
#include "xmpp_tag.h"
#include "console.h"
#include "winetd.h"
#include "cse.h"

// for svc_rw_thread()
typedef struct
{
  SOCKET   *s;
  XMPP_tag tag;
} t_rwstruct;

typedef struct
{
  bool app_mode; //run just like ordinary app
  bool help_show, version_show;
  char cfgfname[MAX_PATH+1]; //config path

  bool install_service;
  bool uninstall_service;
}
t_commandlineparams;

static t_commandlineparams commandlineparams;

static CRITICAL_SECTION cs;

SERVICE_STATUS svc_status;
SERVICE_STATUS_HANDLE hStatus = NULL;

void version_show()
{
  printf(SERVICE_DESCRIPTION_SHORT "\n" SERVICE__VERSION "\n\n" SERVICE_AUTHORS "\n\n");
}

void commandline_help()
{
  version_show();
  printf("Usage:\n" 
         "  winetd options [parameters]\n\n"
         "    options:\n"
         "    --version - shows program version\n"
         "    --config configfile - defines path to config file\n"
         "    --fg - runs as ordinary application (in foreground)\n"
         "    -d, --debug - detailed debug log message mode\n"
         "    -i - install Windows service\n"
         "    -u - uninstall Windows service\n"
         "    /?, -h, --help - shows this help\n"
         );
}

void command_params_load(int argc, char* argv[])
{
  logger::add("Loading and parsing params");

  //defaults
  commandlineparams.cfgfname[0] = '\0';
  commandlineparams.app_mode = false;
  commandlineparams.help_show = false;
  commandlineparams.version_show = false;
  commandlineparams.install_service = false;
  commandlineparams.uninstall_service = false;

  for (int i=1; i < argc; i++)
    {
      if (!strcmp(argv[i], "--config"))
      {
        logger::debug("Config param detected. Trying...");
        //reading config from given file
        if (i+1 >= argc)
          {
            logger::error("Config param must be followed by existing config file path");
            return;
          }
          else
          {
            strcpy(commandlineparams.cfgfname, argv[i+1]);        
            i++;
          }
      }
      else if (!strcmp(argv[i], "-i")) commandlineparams.install_service = true;
      else if (!strcmp(argv[i], "-u")) commandlineparams.uninstall_service = true;
      else if (!strcmp(argv[i], "--fg")) commandlineparams.app_mode = true;
      else if ((!strcmp(argv[i], "/?")) || (!strcmp(argv[i], "-h")) || (!strcmp(argv[i], "--help"))) commandlineparams.help_show = true;
      else if ((!strcmp(argv[i], "--debug")) || (!strcmp(argv[i], "-d"))) logger::debug_mode_set(true);
      else if (!strcmp(argv[i], "--version")) commandlineparams.version_show = true;
    }
  
  if (commandlineparams.cfgfname[0] == '\0') 
    {
      // compose full file name
      //char cfgfname[MAX_PATH+1] = "\0";
      GetModuleFileName(NULL, commandlineparams.cfgfname, MAX_PATH);
      char *tmp = strrchr(commandlineparams.cfgfname, '\\');

      if(tmp == NULL)
      {
        logger::errorfmt("Backsplash is not found in %s", commandlineparams.cfgfname);
        return;
      }

      tmp++; // skip last '\'
      strcpy(tmp, CONFIG_FILE);

      tmp = NULL;
    }

  logger::debugfmt("Config file is located at %s", commandlineparams.cfgfname);
}


/*
  Name: main()

  Description: no comment :)

  Parameters:
        [in] argc - argument count.
        [in] argv - argument list.

  Return value:
          ON SUCCESS: 0
          ON   ERROR: !0

  Revision: 31.01.2007
*/

void svc_main_start();

int main(int argc, char* argv[])
{
  InitializeCriticalSection(&cs);

  logger::init();

  command_params_load(argc, argv);

  // run
  if(argc < 2)
  {
    SERVICE_TABLE_ENTRY ServiceTable[] = {{SERVICE_NAME, svc_main}, {NULL, NULL}};
    return StartServiceCtrlDispatcher(ServiceTable);
  }

  // install/uninstall
  char ModuleName[MAX_PATH];
  SC_HANDLE hMgr = NULL, hSvc = NULL; 

  //if(!strcmp(argv[1], "-i"))
  if (commandlineparams.install_service)
  {
    if(!(hMgr = OpenSCManager(NULL, NULL, SC_MANAGER_CREATE_SERVICE))) return 0;
    GetModuleFileName(NULL, ModuleName, MAX_PATH);
    if(!(hSvc = CreateService(hMgr, SERVICE_NAME, SERVICE_DISPLAY_NAME, SERVICE_CHANGE_CONFIG, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START, SERVICE_ERROR_IGNORE, ModuleName, NULL, NULL, NULL, NULL, NULL))) return 0;

    // determine system UI language (for service description)
    HINSTANCE hDLL = LoadLibrary("kernel32.dll");
    lpGetSystemDefaultUILanguage GetSystemDefaultUILanguage = (lpGetSystemDefaultUILanguage) GetProcAddress(hDLL, "GetSystemDefaultUILanguage");

    SERVICE_DESCRIPTIONA sd;
    
    if(0x419 == GetSystemDefaultUILanguage())
    {
      // Russian
      sd.lpDescription = SERVICE_DESCRIPTION_RU;
    }
    else
    {
      // Default: English
      sd.lpDescription = SERVICE_DESCRIPTION_EN;
    }

    ChangeServiceConfig2(hSvc,SERVICE_CONFIG_DESCRIPTION,&sd);

    CloseServiceHandle(hSvc);
  }
  //else if(!strcmp(argv[1], "-u"))
  if (commandlineparams.uninstall_service)
  {
    if(!(hMgr = OpenSCManager(NULL, NULL, SC_MANAGER_CREATE_SERVICE))) return 0;
    if(!(hSvc = OpenService(hMgr, SERVICE_NAME, DELETE))) return 0;
    if(!DeleteService(hSvc)) return 0;
    CloseServiceHandle(hSvc);
  }  

  if (commandlineparams.version_show)
    {
      version_show();
    }

  if (commandlineparams.help_show)
    {
      commandline_help();
      return 0;
    }

  if (commandlineparams.app_mode)
  {
    svc_main_start();
  }

  return 0;
}

void svc_main_start()
{
  // service works here
  HANDLE h_main_thread = (HANDLE)_beginthread(svc_main_thread, 0, NULL);
  WaitForSingleObject(h_main_thread, INFINITE);
  CloseHandle(h_main_thread);

  logger::close();
}

/*
  Name: svc_main()

  Description: service main thread.

  Parameters:
        [in] dwArgc - argument count.
        [in] pszArgv - argument list.

  Return value: none.

  Revision: 31.01.2007
*/
void WINAPI svc_main(DWORD dwArgc, LPTSTR *pszArgv)
{
  EnterCriticalSection(&cs);

  if(NULL == (hStatus = RegisterServiceCtrlHandler(SERVICE_NAME, svc_handler))) return;

  svc_status.dwServiceType  = SERVICE_WIN32_OWN_PROCESS;
  svc_status.dwControlsAccepted = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
  svc_status.dwWin32ExitCode = NO_ERROR;
  svc_status.dwServiceSpecificExitCode = 0;

  // Here you may start a thread that sends
  // SetServiceStatus(SERVICE_START_PENDING).
  // This is necessary only if service initializes >1 sec.
  svc_status.dwCurrentState = SERVICE_START_PENDING;
  svc_status.dwCheckPoint = 0;
  svc_status.dwWaitHint = 2000;

  SetServiceStatus(hStatus, &svc_status);
  
  // initialization (create additional threads, etc.)
  // END: initialization (create additional threads, etc.)

  // report to SCM that service is running
  svc_status.dwCurrentState = SERVICE_RUNNING;
  svc_status.dwCheckPoint = 0;
  svc_status.dwWaitHint = 0;

  SetServiceStatus(hStatus, &svc_status);

  LeaveCriticalSection(&cs);

  svc_main_start();

  // exitting - report: "stop service"
  svc_status.dwWin32ExitCode = NO_ERROR;
  svc_status.dwCurrentState = SERVICE_STOPPED;

  SetServiceStatus(hStatus, &svc_status);

  return;
}

/*
  Name: svc_handler()

  Description: handles service status and sets service state.

  Parameters:
        [in] Code - service status code.

  Return value: none.

  Revision: 31.01.2007
*/
void WINAPI svc_handler(DWORD Code)
{
  EnterCriticalSection(&cs);

    switch(Code) 
    { 
        case SERVICE_CONTROL_STOP:
    case SERVICE_CONTROL_SHUTDOWN:
            svc_status.dwCurrentState  = SERVICE_STOPPED; 
            svc_status.dwWin32ExitCode = 0; 
            svc_status.dwCheckPoint    = 0; 
            svc_status.dwWaitHint      = 0;
      break;

        case SERVICE_CONTROL_INTERROGATE:
            break;
 
        default:
      break;
    } 
 
    SetServiceStatus(hStatus, &svc_status);

  LeaveCriticalSection(&cs);

    return;
}

/*
  Name: svc_main_thread()

  Description: service main thread - reads config data and
         launches threads-listeners.

  Parameters:
        [in] tParam - not used.

  Return value: none.

  Revision: 30.01.2007
*/
void __cdecl svc_main_thread(LPVOID tParam)
{
  logger::add("======================8<===cut==========================");
  logger::add("STARTING Winetd...");

  // read config file
  HANDLE hConfigFile = INVALID_HANDLE_VALUE;

  hConfigFile = CreateFile(commandlineparams.cfgfname, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

  if(hConfigFile == INVALID_HANDLE_VALUE)
  {
    logger::add("-ERR Error opening config file: no config file found!");
    return;
  }

  DWORD confsize = GetFileSize(hConfigFile, NULL);

  if(confsize == INVALID_FILE_SIZE)
  {
    logger::add("-ERR Error opening config file: invalid file size!");
    CloseHandle(hConfigFile);
    return;
  }

  char *confbuf = new char[confsize + 1];

  DWORD nRead = 0;

  if(false == ReadFile(hConfigFile, confbuf, confsize, &nRead, NULL))
  {
    logger::add("-ERR Error reading config file!");
    CloseHandle(hConfigFile);
    return;
  }

  if(nRead == 0)
  {
    logger::add("-ERR Error reading config file: file has zero size!");
    CloseHandle(hConfigFile);
    return;
  }

  CloseHandle(hConfigFile);

  logger::add("+OK Config file was read.");

  // parse config file

  XMPP_tag config;

  // skip "<?xml ... ?>"
  char *tmp = strstr(confbuf, "<?xml");

  if((NULL == tmp) || (0 == (*tmp)))
  {
    logger::add("-ERR Error parsing config file: not a valid XML file!");
    return;
  }

  tmp = strstr(tmp, "?>");

  if((NULL == tmp) || (0 == (*tmp)))
  {
    logger::add("-ERR Error parsing config file: not a valid XML file!");
    return;
  }

  tmp += 2; // skip "?>"
  // END: skip "<?xml ... ?>"

  config.parse(tmp, tmp + lstrlen(tmp));

  delete[]confbuf;
  tmp = NULL;

  if(config.parse_error == true)
  {
    logger::add("-ERR Error parsing config file: not a valid XML file!");
    return;
  }

  logger::add("+OK Config file was parsed.");

  // start listening services
  net::init();

  std::list<XMPP_tag>::iterator i;

  for(i = config.sub_tags->begin(); i != config.sub_tags->end(); i++)
  {
    if(!lstrcmp(i->get_name(), "cmd"))
    {
      HANDLE h_listen_thread = (HANDLE)_beginthread(svc_listen_thread, 0, &(*i));
      // let listening port start
      WaitForSingleObject(h_listen_thread, 1000);
      CloseHandle(h_listen_thread);
    }
  }

  for(;;) Sleep(100);
}

/*
  Name: svc_listen_thread()

  Description: listener thread - listens on iface and port specified in config.

  Parameters:
        [in] tParam - pointer to parsed "<cmd>" tag from config.

  Return value: none.

  Revision: 01.09.2007
*/
void __cdecl svc_listen_thread(LPVOID tParam)
{
  if(NULL == tParam) return;

  CSE::MapSEtoCE(); // Must be called before any exceptions are raised
  logger::debug("svc_listen_thread. Starting listen thread");

  const char *iface=0, *config_iface=0, *config_run=0;
  unsigned short port=0;
  do
    {
      try
        {
          XMPP_tag tag = (*(XMPP_tag *)tParam);

          SOCKET ls, *ss = NULL;

          logger::debug("svc_listen_thread. Getting config params");
          tag.Lock();
          try
            {
              iface = tag.get_attribute(CONFIG_IFACE);
              port = atoi(tag.get_attribute(CONFIG_PORT));
              config_iface=tag.get_attribute(CONFIG_DESCRIPTION); 
              config_run=tag.get_attribute(CONFIG_RUN);
            }
          catch(...)
            {
              tag.Unlock();
              throw;
            }
          tag.Unlock();

          if (!net::bind_and_listen(&ls, iface, port)) 
            {
              logger::errorfmt("Can't bind to %s:%d. Exiting", iface, port);
              break;
            }

          logger::addfmt("'%s' on iface: '%s' on port: '%d' (handler: '%s')",
              config_iface,
              iface, port, config_run);

          logger::debug("svc_listen_thread. Starting listen loop");

          while(ss = net::accept(&ls))
          {
            logger::debug("svc_listen_thread. Connection accepted");
            t_rwstruct *rwstruct = new t_rwstruct;

            rwstruct->s   = ss;
            rwstruct->tag = tag;

            HANDLE h_rw_thread = (HANDLE)_beginthread(svc_rw_thread, 0, rwstruct);
            CloseHandle(h_rw_thread);

            ss = NULL;
          }

          net::disconnect(&ls);
        }
      catch (CSE se) 
      //__except_cse
       {
         logger::errorfmt("svc_listen_thread. Exception happened %X :~(", DWORD(se));
       }
    }
  while (0);
  logger::addfmt("Stopping '%s' on iface: '%s' on port: '%d' (handler: '%s')",
      config_iface,
      iface, port, config_run);
}

/*
  Name: svc_rw_thread()

  Description: read/write thread - interacts console app to socket.

  Parameters:
        [in] tParam - pointer to "t_rwstruct" structure.

  Return value: none.

  Revision: 30.01.2007
*/
void __cdecl svc_rw_thread(LPVOID tParam)
{
  if(NULL == tParam) return;

  CSE::MapSEtoCE(); // Must be called before any exceptions are raised

  DWORD tid = GetCurrentThreadId();
  logger::debugfmt("tid=%d. svc_rw_thread start", tid);

  try
    {
       logger::debugfmt("tid=%d. Getting rwstruct", tid);

       //* (PBYTE) 0 = 0;       // Access violation

       t_rwstruct *rwstruct = (t_rwstruct *)tParam;

       if(*(rwstruct->s) == INVALID_SOCKET)
       {
         if(NULL != rwstruct->s)
         {
           delete rwstruct->s;
           rwstruct->s = NULL;
         }
         
         if(NULL != rwstruct)
         {
           delete rwstruct;
         }
         
         return;
       }

       logger::debugfmt("tid=%d. Getting peer name", tid);
       // Who connected?
       char *peername = net::getpeername(rwstruct->s); 
       // 'peername' will be deleted on disconnect.
       logger::addfmt("tid=%d. Connected: %s", tid , peername);
       // END: Who connected?

       logger::debugfmt("tid=%d. Creating console", tid);
       console con;

       logger::debugfmt("tid=%d. Init console", tid);

       rwstruct->tag.Lock();
       try
         {
           con.init(rwstruct->tag.get_attribute(CONFIG_RUN));
         }
       catch(...)
         {
           rwstruct->tag.Unlock();
           throw;
         }
       rwstruct->tag.Unlock();

       char buf[MAX_STRING];

       // R/W cycle
       while(con.is_alive())
       {
         memset(buf, 0, sizeof(buf));
         // con -> socket
         logger::debugfmt("tid=%d. con -> socket", tid);
         if(!con.read(buf, sizeof(buf)-1)) 
           {
             logger::errorfmt("tid=%d. con -> socket failed", tid);
             break;
           }
         OemToCharA(buf, buf);

         logger::debugfmt("tid=%d. sendall", tid);
         if(!net::sendall(rwstruct->s, buf, lstrlen(buf))) 
           {
             logger::errorfmt("tid=%d. sendall failed", tid);
             break;
           }

         memset(buf, 0, sizeof(buf));

         // socket -> con
         logger::debugfmt("tid=%d. socket -> con", tid);
         if((-1) == net::recvall(rwstruct->s, buf, sizeof(buf)-1)) 
           {
             logger::errorfmt("tid=%d. socket -> con failed", tid);
             break;
           }
         CharToOemA(buf, buf);

         logger::debugfmt("tid=%d. con.write", tid);
         if(!con.write(buf, lstrlen(buf))) 
           {
             logger::errorfmt("tid=%d. con.write failed", tid);
             break;
           }
       }

       // Who disconnected?
       logger::addfmt("tid=%d. Disconnected: %s", tid , peername);
       
       delete[]peername; peername = NULL;

       logger::debugfmt("tid=%d. Console destroy", tid);
       con.destroy();

       logger::debugfmt("tid=%d. Socket disconnect", tid);
       net::disconnect(rwstruct->s);

       logger::debugfmt("tid=%d. Free socket structures", tid);
       if(NULL != rwstruct->s)
       {
         delete rwstruct->s;
         rwstruct->s = NULL;
       }
       
       if(NULL != rwstruct)
       {
         delete rwstruct;
       }

       logger::debugfmt("tid=%d. Connection thread finished", tid);
    }
  catch (CSE se) 
   {
      logger::errorfmt("tid=%d. svc_rw_thread. Exception happened %X :~(", tid, DWORD(se));
      /*
      switch (se)     // Calls the operator DWORD() member function
      {
         case EXCEPTION_ACCESS_VIOLATION:
            // This code handles an access-violation exception

            logger::debugfmt("tid=%d. AV happened :~(", tid);
            break;
         case EXCEPTION_INT_DIVIDE_BY_ZERO:
            // This code handles a division-by-zero exception
            break;
         default:
            // We don't handle any other exceptions
            throw;   // Maybe another catch is looking for this
            break;   // Never executes
      }
      */
   }

  logger::debugfmt("tid=%d. Connection thread close", tid);
  _endthread();
}
