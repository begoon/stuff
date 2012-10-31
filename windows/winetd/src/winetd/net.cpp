/*
  NET - just handy wrapper for Winsock32 and WinInet.
  by Alexander S. Pristenski 2007

  Change log:

  16.01.2007  Alexander S. Pristenski - initial creation.
  30.01.2007  Alexander S. Pristenski - feature: getpeername() added.
  01.09.2007  Alexander S. Pristenski - feature: setup of network interface to listen on added.
*/

#include <winsock2.h>
#include <wininet.h>
#include "net.h"

#include <stdio.h>

#pragma comment (lib,"ws2_32.lib")
#pragma comment (lib,"wininet.lib")
#pragma comment (lib,"advapi32.lib")

/* --------------------- local defines -------------------- */

#ifndef SIO_KEEPALIVE_VALS
#define SIO_KEEPALIVE_VALS _WSAIOW(IOC_VENDOR,4)
#endif

#ifdef LOG_TO_FILE
#define LOG_FNAME "netlog.txt"
static unsigned long connectn = 0;
#endif

// default SOCKET keepalives
#define KEEPALIVE_TIME_1 60000
#define KEEPALIVE_TIME_2 1000

// HTTP defines
#define HTTP_BUF_SIZE     1024
#define HTTP_USER_AGENT     "Mozilla"
#define HTTP_USER_AGENT_VERSION "4.0"
#define HTTP_PORT_NUMBER    "80"
#define HTTP_TIMEOUT      30

net::t_last_err net::last_err = net::OK;

/* ----------------- END: local defines ------------------- */

/* ----------------- Winsock32 functions ------------------ */

/*
  Name: init()

  Description: inits Winsock32 and WinInet engines.

  Parameters: none.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::init(void)
{
  LoadLibrary("WS2_32.dll");
  LoadLibrary("wininet.dll");

  WSADATA WSAData;
  if(WSAStartup (MAKEWORD(2,0), &WSAData) != 0) return false;
  return true;
}

/*
  Name: enum_ips()

  Description: enumerates all IP addresses in the system
         like ipconfig utility does.

  Parameters:
         [out] char *buf - buffer for string with IPs.
          [in] unsigned long buf_size - size of buf.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::enum_ips(char *buf, unsigned long buf_size)
{
  if(buf != NULL) buf[0] = '\0'; // empty string
  
  SOCKET s;

  if((s = socket(AF_INET, SOCK_STREAM, IPPROTO_IP)) == INVALID_SOCKET)
  {
    net::disconnect(&s);
    return false;
  }

  DWORD bytesret;
  SOCKET_ADDRESS_LIST addrlist[100]; // 100 IPs - I think enough! :)

  if(SOCKET_ERROR == WSAIoctl(s, SIO_ADDRESS_LIST_QUERY, NULL, 0, &addrlist, sizeof(addrlist), &bytesret, 0, 0))
  {
    net::disconnect(&s);
    return false;
  }

  // No IPs - perhaps we're not connected.
  if(0 == addrlist[0].iAddressCount)
  {
    net::disconnect(&s);
    return false;
  }

  for(int i=0; i < addrlist[0].iAddressCount; i++)
  {
    if((buf != NULL) && ((lstrlen(buf) + lstrlen(inet_ntoa(((sockaddr_in *)(addrlist[0].Address[i].lpSockaddr))->sin_addr)) + 2) < buf_size))
    {
      lstrcat(buf, inet_ntoa(((sockaddr_in *)(addrlist[0].Address[i].lpSockaddr))->sin_addr));
      lstrcat(buf, " ");
    }
  }

  net::disconnect(&s);
  return true;
}

/*
  Name: get_ie_proxy_settings()

  Description: retrieves proxy settings ("host:port")
         of Internet Explorer from the registry.

  Parameters:
         [out] char *buf - buffer for string proxy settings.
          [in] unsigned long buf_size - size of buf.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::get_ie_proxy_settings(char *buf, unsigned long buf_size)
{
  HKEY hKey;
  char temp_buf[MAX_PATH];
  char *index1 = NULL;
  char *index2 = NULL;

  if(ERROR_SUCCESS != RegOpenKeyEx(HKEY_CURRENT_USER, "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings", 0, KEY_QUERY_VALUE, &hKey)) return false;
  if(ERROR_SUCCESS != RegQueryValueEx(hKey, "ProxyServer", NULL, NULL, (unsigned char *)temp_buf, &buf_size)) return false;

  // temp_buf can be ether "proxy:port" or "...http=proxy:port;..."
  if(NULL == strstr(temp_buf, "=")) // perhaps it's single proxy for all
  {
    lstrcpy(buf, temp_buf);
  }
  else // try to find proxy for http
  {
    if(NULL == (index1 = strstr(temp_buf, "http="))) return false;
    index1 += lstrlen("http=");
    // the "http=proxy:port" can end with "\0" or ";"
    if(NULL != (index2 = strstr(index1, ";")))
    {
      *index2 = '\0';
      lstrcpy(buf, index1);
    }
    else // ends with "\0"
    {
      lstrcpy(buf, index1);
    }
  }
  
  RegCloseKey(hKey);
  return true;
}

/*
  Name: connect_multi()

  Description: trys to connect directly and (if fails)
         then via proxy.

  Parameters:
         [in] SOCKET *s - pointer to socket that exists, but
                          not created with socket() call yet.
         [in] char *server - target server.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::connect_multi(SOCKET *s, char *server)
{
  // try to connect directly
  if(net::connect(s, server)) return true;
  // then via proxy from IE settings
  char proxy[MAX_PATH] = "\0";
  if(net::get_ie_proxy_settings(proxy, sizeof(proxy)))
  {
    if(net::connect_via_http_proxy(s, proxy, server)) return true;
  }
  return false;
}

/*
  Name: connect()

  Description: trys to connect directly to target server.

  Parameters:
         [in] SOCKET *s - pointer to socket that exists, but
                          not created with socket() call yet.
         [in] char *server - target server.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::connect(SOCKET *s, char *server)
{
  // check server parameter
  if(NULL == server) return false;
  if('\0' == *server) return false;

  // parse server = "host:port"
  char host[MAX_PATH] = "\0";
  char port[MAX_PATH] = "\0";

  char *pbeg = server;
  char *pcur = server;

  while(':' != *(++pcur));
  memcpy(host, pbeg, (pcur-pbeg));
  pcur++; // omit ':'
  pbeg = pcur; // begin with port
  while(0 != *(++pcur));
  memcpy(port, pbeg, (pcur-pbeg));

  // connect
  SOCKADDR_IN    addr;
  struct hostent *hp = NULL;

  memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port   = htons(atoi(port));

    if (IsCharAlpha(server[0]))
  {
    hp = gethostbyname(host);
    }
    else
  {
    unsigned int ip_addr = inet_addr(host);
    hp = gethostbyaddr((char *)&ip_addr,4,AF_INET);
    }

  if(hp == NULL) return false; // can't resolve
  
    memcpy(&(addr.sin_addr), hp->h_addr, hp->h_length);

  if((*s = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) return false;

  if(::connect(*s, (PSOCKADDR) &addr, sizeof(addr)) == SOCKET_ERROR)
  {
    net::disconnect(s);
    return false;
  }

#ifdef LOG_TO_FILE
  HANDLE hLogFile;

  // try to open existing
  hLogFile = CreateFile(LOG_FNAME, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

  if(INVALID_HANDLE_VALUE == hLogFile)
  {
    // anyway - close handle from previous try!
    CloseHandle(hLogFile);
    // try to create file
    hLogFile = CreateFile(LOG_FNAME, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  }

  if(INVALID_HANDLE_VALUE != hLogFile)
  {
    connectn++;
    char delimiter[1024];
    wsprintf(delimiter, "%s %lu %s", "\n\n<------| CONNECT: ", connectn ,"|------>\n\n");
    DWORD written;
    SetFilePointer(hLogFile, 0, NULL, FILE_END);
    WriteFile(hLogFile, delimiter, lstrlen(delimiter), &written, NULL);
  }

  CloseHandle(hLogFile);
#endif

  return true;
}

/*
  Name: bind_and_listen()

  Description: binds socket and switches it to listening mode.

  Parameters:
         [in] SOCKET *s - pointer to socket that exists, but
                          not created with socket() call yet.
         [in] const char *iface - network interface to listen on.
         [in] unsigned short port - port to listen on.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 01.09.2007
*/
bool net::bind_and_listen(SOCKET *s, const char *iface, unsigned short port)
{
  SOCKADDR_IN server_addr;

  if((*s = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET) return false;

  bool opt = true;
  if(SOCKET_ERROR == setsockopt(*s, SOL_SOCKET, SO_REUSEADDR, (char *)&opt, sizeof(opt))) return false;

  server_addr.sin_family      = AF_INET;
  server_addr.sin_addr.s_addr = inet_addr(iface);
  server_addr.sin_port        = htons(port);
  if(bind(*s, (struct sockaddr *) &server_addr, sizeof(server_addr)) == SOCKET_ERROR) return false;

  if(listen(*s, SOMAXCONN) == SOCKET_ERROR) return false;

  return true;
}

/*
  Name: accept()

  Description: accepts incoming TCP connection on server socket.

  Parameters:
         [in] SOCKET *s - already listening server socket.

  Return value:
          ON SUCCESS - temporary socket to work with client.
          ON   ERROR - NULL.

  Revision: 16.01.2007
*/
SOCKET* net::accept(SOCKET *s)
{
  int     accepted_len = sizeof(SOCKADDR_IN);
  SOCKADDR_IN temp_addr;
  SOCKET    *server_socket = new SOCKET;

  if(server_socket == NULL) return NULL;

  if((*server_socket = ::accept(*s, (struct sockaddr *) &temp_addr, (int *)&accepted_len)) == INVALID_SOCKET)
  {
    // INVALID_SOCKET
    net::disconnect(s);
    if(server_socket != NULL) delete server_socket;
    server_socket = NULL;
  }

  if(!net::keepalive_on(server_socket, KEEPALIVE_TIME_1, KEEPALIVE_TIME_2))
  {
    net::disconnect(s);
    if(server_socket != NULL) delete server_socket;
    server_socket = NULL;
  }

  return server_socket;
}

/*
  Name: getpeername()

  Description: retrieves the name of the peer (in 'host:port' format)
         to which a socket is connected.

  Parameters:
         [in] SOCKET *s - already connected (accepted).

  Return value:
          ON SUCCESS - peer name string.
          ON   ERROR - NULL.

  Revision: 30.01.2007
*/
char* net::getpeername(SOCKET *s)
{
  SOCKADDR peer;
  int      peerlen  = sizeof(peer);
    
  if(0 == ::getpeername(*s, &peer, &peerlen))
  {
    char *peername = new char[22]; // IPv4 + ':' + port + '\0'
    wsprintf(peername, "%s:%i", inet_ntoa(((SOCKADDR_IN *)&peer)->sin_addr), ntohs(((SOCKADDR_IN *)&peer)->sin_port));
    return peername;
  }

  return NULL;
}

/*
  Name: connect_via_http_proxy()

  Description: connects to target server via HTTP proxy.

  Parameters:
         [in] SOCKET *s - pointer to socket that exists, but
                          not created with socket() call yet.
         [in] char *proxy - proxy settings in "host:port" format.
         [in] char *server - target server to connect to.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::connect_via_http_proxy(SOCKET *s, char *proxy, char *server)
{
  // check proxy & server parameters
  if((NULL == server) || (NULL == proxy)) return false;
  if(('\0' == *server) || ('\0' == *proxy)) return false;

  // buffer
  char Buf[HTTP_BUF_SIZE];

  // form CONNECT request
  wsprintf(Buf, "CONNECT %s %s\r\n", server, HTTP_VERSION);
  // form "User-Agent: " part
  lstrcat(Buf, "User-Agent: ");
  lstrcat(Buf, HTTP_USER_AGENT);
  lstrcat(Buf, "/");
  lstrcat(Buf, HTTP_USER_AGENT_VERSION);
  lstrcat(Buf, "\r\n\r\n");

  // connect to proxy server
  if(!net::connect(s, proxy)) return false;

  // send request to server
  if(!net::sendall(s, Buf, lstrlen(Buf)))
  {
    net::disconnect(s);
    return false;
  }

  // WORKAROUND: make small delay between send header and receive response
  Sleep(3000); // 3 sec. is enough I think
  
  // empty buffer
  memset(Buf, 0, sizeof(Buf));

  // get proxy response byte-by-byte
  unsigned short newlines = 0; // header-end condition is 2 newlines
  long bytes_read = 0;
  char *index = &Buf[0];

  while(newlines != 2)
  {
    if((-1) == net::recvall(s, index, 1))
    {
      net::disconnect(s);
      return false;
    }

    bytes_read++;

    if(bytes_read == HTTP_BUF_SIZE)
    {
      net::disconnect(s);
      return false;
    }

    if(*index == '\r')
    {
      index++; // don't increment newlines
      continue;
    }
    else if(*index == '\n') // LF
    {
      newlines++;
    }
    else
    {
      newlines = 0;
    }
    index++;
  }
  index -=2; // snip trailing CRLF
  *index = '\0';

  // parse header received and check error code
  int error_code;

  if(NULL == (index = strstr(Buf, "HTTP/")))
  {
    net::disconnect(s);
    return false;
  }

  while(*index != ' ') index++;
  index++;

  error_code = atoi(index);

  if(error_code<200 || error_code>299)
  {
    net::disconnect(s);
    return false;
  }

  return true;
}

/*
  Name: keepalive_on()

  Description: sets keepalive values for the socket.
         Very useful to detect broken TCP connections
         without send/receive data calls.

  Parameters:
         [in] SOCKET *s - pointer to the socket.
         [in] u_long time - time between keepalive packet send.
         [in] u_long interval - interval between keepalive packets.

  Return value:
          ON SUCCESS - true.
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::keepalive_on(SOCKET *s, u_long time, u_long interval)
{
  struct tcp_keepalive
  {
    u_long  onoff;
    u_long  keepalivetime;
    u_long  keepaliveinterval;
  } alive;

  alive.onoff       = 1;
  alive.keepalivetime   = time;
  alive.keepaliveinterval = interval;

  DWORD dwSize;

  if(SOCKET_ERROR == WSAIoctl(*s, SIO_KEEPALIVE_VALS, &alive, sizeof(alive), NULL, 0, &dwSize, NULL, NULL))
  {
    return false;
  }
  return true;
}

/*
  Name: recv()

  Description: receives data from a connected socket.

  Parameters:
          [in] SOCKET *s - pointer to connected socket.
         [out] char *buf - buffer for incoming data.
          [in] unsigned long buf_size - size of buf.

  Return value:
          ON SUCCESS - number of bytes received.
          ON   ERROR - '-1'.

  IMPORTANT NOTE: this is a blocking call, i.e. it returns
          only on data received!

  Revision: 16.01.2007
*/
long net::recv(SOCKET *s, char *buf, unsigned long buf_size)
{
  if(INVALID_SOCKET == *s) return -1;

  long received = 0;

  if(SOCKET_ERROR == (received = ::recv(*s, buf, buf_size, 0)))
  {
    net::disconnect(s);
    return -1;
  }
  // if connection has been gracefully closed
  if(0 == received)
  {
    net::disconnect(s);
    return -1;
  }

#ifdef LOG_TO_FILE
  
  DWORD written;
  DWORD counter = 0;
  HANDLE hLogFile = CreateFile(LOG_FNAME, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if((INVALID_HANDLE_VALUE != hLogFile) && (buf[0]))
  {
    SetFilePointer(hLogFile, 0, NULL, FILE_END);
    WriteFile(hLogFile, "\n<- ", lstrlen("\n<- "), &written, NULL);
    while(counter < received)
    {
      WriteFile(hLogFile, &buf[counter], 1, &written, NULL);
      if((buf[counter] == '\n') && ((counter+1) != received))
      {
        WriteFile(hLogFile, "<- ", lstrlen("<- "), &written, NULL);
      }
      counter++;
    }
  }
    
  CloseHandle(hLogFile);
#endif

  return received;
}

/*
  Name: recvall()

  Description: receives data from a connected socket.

  Parameters:
          [in] SOCKET *s - pointer to connected socket.
         [out] char *buf - buffer for incoming data.
          [in] unsigned long buf_size - size of buf.

  Return value:
          ON SUCCESS - number of bytes received.
          ON   ERROR - '-1'.

  IMPORTANT NOTE: this is a nonblocking call which uses
          socket polling via select() call.

  Revision: 16.01.2007
*/
long net::recvall(SOCKET *s, char *buf, unsigned long buf_size)
{
  timeval time_out;
  time_out.tv_sec  = 0;
  time_out.tv_usec = 5000; // 0.005 сек.
  
  fd_set  read_s;

  if(INVALID_SOCKET == *s) return -1;

  FD_ZERO(&read_s);
  FD_SET(*s, &read_s);
  long received = 0;

  if(SOCKET_ERROR == select(0, &read_s, NULL, NULL, &time_out)) return -1;

  if(FD_ISSET(*s, &read_s))
  {
    if(SOCKET_ERROR == (received = ::recv(*s, buf, buf_size, 0)))
    {
      net::disconnect(s);
      return -1;
    }
    // if connection has been gracefully closed
    if(0 == received)
    {
      net::disconnect(s);
      return -1;
    }
  }

#ifdef LOG_TO_FILE
  DWORD written;
  DWORD counter = 0;
  HANDLE hLogFile = CreateFile(LOG_FNAME, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if((INVALID_HANDLE_VALUE != hLogFile) && (buf[0]))
  {
    SetFilePointer(hLogFile, 0, NULL, FILE_END);
    WriteFile(hLogFile, "\n<- ", lstrlen("\n<- "), &written, NULL);
    while(counter < received)
    {
      WriteFile(hLogFile, &buf[counter], 1, &written, NULL);
      if((buf[counter] == '\n') && ((counter+1) != received))
      {
        WriteFile(hLogFile, "<- ", lstrlen("<- "), &written, NULL);
      }
      counter++;
    }
  }
  CloseHandle(hLogFile);
#endif

  return received;
}

/*
  Name: sendall()

  Description: sends data to a connected socket.

  Parameters:
         [in] SOCKET *s - pointer to connected socket.
         [in] char *buf - buffer for outgoing data.
         [in] unsigned long buf_size - size of data to send.

  Return value:
          ON SUCCESS - true (_all_ the data sent).
          ON   ERROR - false.

  Revision: 16.01.2007
*/
bool net::sendall(SOCKET *s, const char *buf, unsigned long left)
{
  unsigned long idx = 0;
  int res;

  if(INVALID_SOCKET == *s) return false;

  while(left > 0)
  {
    if(SOCKET_ERROR == (res = send(*s, &buf[idx], left, 0)))
    {
      net::disconnect(s);
      return false;
    }

#ifdef LOG_TO_FILE
  DWORD written;
  HANDLE hLogFile = CreateFile(LOG_FNAME, GENERIC_WRITE, (FILE_SHARE_READ | FILE_SHARE_WRITE), NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if((INVALID_HANDLE_VALUE != hLogFile) && (buf[0]))
  {
    SetFilePointer(hLogFile, 0, NULL, FILE_END);
    WriteFile(hLogFile, "\n-> ", lstrlen("\n-> "), &written, NULL);
    WriteFile(hLogFile, &buf[idx], res, &written, NULL);
  }
  CloseHandle(hLogFile);
#endif

    left -= res;
    idx  += res;
  }

  return true;
}

/*
  Name: disconnect()

  Description: disconnects connected socket.

  Parameters:
         [in] SOCKET *s - pointer to connected socket.

  Return value: none.

  Revision: 16.01.2007
*/
void net::disconnect(SOCKET *s)
{
  if (INVALID_SOCKET == *s) return;

  ::shutdown(*s, SD_BOTH);
  ::closesocket(*s);
  *s = INVALID_SOCKET;
}

/* ----------------- END: Winsock32 functions ------------- */


/* ----------------- WinInet functions -------------------- */

/*
  Name: hget()

  Description: gets data from URL and places it to local file.

  Example: hget("http://www.server.org/file.ext c:\file.ext");

  Parameters:
         [in] void *param - see example.

  Return value:
          ON SUCCESS - '1'.
          ON   ERROR - '0'.

  Revision: 16.01.2007
*/
unsigned long __stdcall net::hget(void *param)
{
  char *par = (char *)param;

  InterlockedExchange((long *)&net::last_err, (long)net::WAIT);

  if(param == NULL)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }

  if(*(char *)param == 0)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }

  // get hostname
  char *host = strstr(par, "http://");
  if(NULL == host) // "http://" prefix not found
  {
    host = par;
  }
  else
  {
    host = par + lstrlen("http://");
  }

  char *hostfile = host;
  while((*hostfile != 0x00) && (*hostfile != '/')) hostfile++;
  if(*hostfile == 0x00)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0; // no file specified
  }
  if(*hostfile == '/') *hostfile = 0x00;
  if(*(++hostfile) == 0x00)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0; // no file specified after 'host/'
  }

  // put 0x00 after hostfile and locate localfile if specified
  char *localfile = hostfile;
  while((*localfile != 0x00) && (*localfile != 0x20)) localfile++;
  
  if(*localfile != 0x00) // perhaps localfile specified
  {
    *localfile = 0x00; // it's 1st space after full hostfile name
    localfile++;
    // skip spaces before full local file path
    while((*localfile != 0x00) && (*localfile == 0x20)) localfile++;
    if(*localfile == 0x00) localfile = NULL; // no localfile specified
  }
  else
  {
    localfile = NULL; // no localfile specified
  }

  HINTERNET hInternet = ::InternetOpen(HTTP_USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
  if(hInternet == NULL)
  {
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

    HINTERNET hConnect = ::InternetConnect(hInternet, host, INTERNET_DEFAULT_HTTP_PORT, NULL, NULL, INTERNET_SERVICE_HTTP, 0, 0);
  if(hConnect == NULL)
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  HINTERNET hRequest = ::HttpOpenRequest(hConnect, "GET", hostfile, NULL, NULL, 0, 0, /*(INTERNET_FLAG_PRAGMA_NOCACHE | INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_NO_UI),*/ 0);
  if(hRequest == NULL)
  {
    ::InternetCloseHandle(hRequest);
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  BOOL bSend = ::HttpSendRequest(hRequest, NULL, 0, NULL, 0);
  if(bSend == false)
  {
    ::InternetCloseHandle(hRequest);
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  DWORD content_length = 0;
  DWORD content_length_size = sizeof(content_length);
  DWORD header_index = 0;
  
  BOOL bInfo = ::HttpQueryInfo(hRequest, (HTTP_QUERY_CONTENT_LENGTH | HTTP_QUERY_FLAG_NUMBER), &content_length, &content_length_size, &header_index);
  if(bInfo == false)
  {
    ::InternetCloseHandle(hRequest);
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  char *downloaded_file = new char[(content_length + 1)]; // +1 for zer0 at tail

  if(downloaded_file == NULL)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }

  memset(downloaded_file, 0, (content_length + 1));

  if(NULL == downloaded_file)
  {
    ::InternetCloseHandle(hRequest);
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

    DWORD dwBytesRead;

    BOOL bRead = ::InternetReadFile(hRequest, downloaded_file, content_length, &dwBytesRead);

    if ((bRead == FALSE) || (dwBytesRead == 0) || (dwBytesRead != content_length))
  {
    if(downloaded_file != NULL) delete[]downloaded_file;
    ::InternetCloseHandle(hRequest);
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  if(downloaded_file == NULL)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  char local_fname[MAX_PATH] = "\0";

  if(localfile == NULL) // no localfile specified
  {
    // find file name only
    char *fname = hostfile + lstrlen(hostfile);
    while((fname != hostfile) && (*fname != '/')) fname--;
    if(fname != hostfile) fname++;

    GetCurrentDirectory(MAX_PATH, local_fname);
    lstrcat(local_fname, fname);
  }
  else // localfile specified
  {
    lstrcpyn(local_fname, localfile, MAX_PATH);
  } 

  // write file
  HANDLE hFile;
  DWORD written;

  if(INVALID_HANDLE_VALUE != (hFile = CreateFile(local_fname, GENERIC_WRITE, FILE_SHARE_READ, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL)))
  {
    if(false == WriteFile(hFile, downloaded_file, content_length, &written, NULL))
    {
      CloseHandle(hFile);
      DeleteFile(local_fname); // delete file if write error occurs...
    }
    else
    {
      CloseHandle(hFile);
    }
  }

  // free buffer
  if(NULL != downloaded_file)
  {
    if(downloaded_file != NULL) delete[]downloaded_file;
    downloaded_file = NULL;
  }

  ::InternetCloseHandle(hRequest);
  ::InternetCloseHandle(hConnect);
  ::InternetCloseHandle(hInternet);
  InterlockedExchange((long *)&net::last_err, (long)net::OK);
  if(par != NULL) delete[]par;
  return 1;
}

/*
  Name: fput()

  Description: puts local file to ftp.

  Example: fput("ftp://user:pass@ftp.server.org/dir/file.ext c:\file.ext");

  Parameters:
         [in] void *param - see example.

  Return value:
          ON SUCCESS - '1'.
          ON   ERROR - '0'.

  Revision: 16.01.2007
*/
unsigned long __stdcall net::fput(void *param)
{
  char *par = (char *)param;

  InterlockedExchange((long *)&net::last_err, (long)net::WAIT);

  if(param == NULL)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }

  if(*(char *)param == 0)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }

  char root_dir[] = "/";

  // user
  char *user = strstr(par, "ftp://");
  if(NULL == user)
  {
    user = par;
  }
  else
  {
    user = par + lstrlen("ftp://");
  }

  // pass
  char *pass = user;
  while((*pass != 0x00) && (*pass != ':')) pass++;
  if(*pass == ':') // delimiter ':' found
  {
    *pass = 0x00;
    pass++;
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  // host
  char *host = pass;
  while((*host != 0x00) && (*host != '@')) host++;
  if(*host == '@') // delimiter ':' found
  {
    *host = 0x00;
    host++;
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  // dir
  char *dir = host;
  while((*dir != 0x00) && (*dir != '/')) dir++;
  if(*dir == '/')
  {
    *dir = 0x00;
    dir++;
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  // files
  char *remote_file = dir;
  char *local_file  = dir;
  // find 1st space/tab
  while((*remote_file != 0x00) && (*remote_file != 0x20)) remote_file++;
  if(*remote_file == 0x20)
  {
    local_file = remote_file;
    // skip the space
    *local_file = 0x00;
    local_file++;
    // go backward to find 1st '/' from the end
    while((remote_file != dir) && (*remote_file != '/')) remote_file--;
    if(*remote_file == '/')
    {
      *remote_file = 0x00;
      remote_file++;
    }
    else
    {
      dir = root_dir;
    }
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  HINTERNET hInternet = ::InternetOpen(HTTP_USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
  if(hInternet == NULL)
  {
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

    HINTERNET hConnect = ::InternetConnect(hInternet, host, INTERNET_INVALID_PORT_NUMBER, user, pass, INTERNET_SERVICE_FTP, INTERNET_FLAG_PASSIVE, 0);
  if(hConnect == NULL)
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  if(!FtpSetCurrentDirectory(hConnect, dir))
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }
  
  // NOTE: existing file with the same name on FTP server will be rewritten!
  if(!FtpPutFile(hConnect, local_file, remote_file, (INTERNET_FLAG_RELOAD | FTP_TRANSFER_TYPE_BINARY), 0))
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  ::InternetCloseHandle(hConnect);
  ::InternetCloseHandle(hInternet);
  InterlockedExchange((long *)&net::last_err, (long)net::OK);
  if(par != NULL) delete[]par;
  return 1;
}

/*
  Name: fdel()

  Description: deletes file from ftp.

  Example: fdel("ftp://user:pass@ftp.server.org/dir/file.ext");

  Parameters:
         [in] void *param - see example.

  Return value:
          ON SUCCESS - '1'.
          ON   ERROR - '0'.

  Revision: 16.01.2007
*/
unsigned long __stdcall net::fdel(void *param)
{
  char *par = (char *)param;

  InterlockedExchange((long *)&net::last_err, (long)net::WAIT);

  if(par == NULL)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }

  if(*(char *)par == 0)
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    return 0;
  }
  
  char root_dir[] = "/";

  // user
  char *user = strstr(par, "ftp://");
  if(NULL == user)
  {
    user = par;
  }
  else
  {
    user = par + lstrlen("ftp://");
  }

  // pass
  char *pass = user;
  while((*pass != 0x00) && (*pass != ':')) pass++;
  if(*pass == ':') // delimiter ':' found
  {
    *pass = 0x00;
    pass++;
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  // host
  char *host = pass;
  while((*host != 0x00) && (*host != '@')) host++;
  if(*host == '@') // delimiter ':' found
  {
    *host = 0x00;
    host++;
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  // dir
  char *dir = host;
  while((*dir != 0x00) && (*dir != '/')) dir++;
  if(*dir == '/')
  {
    *dir = 0x00;
    dir++;
  }
  else
  {
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  // file
  char *remote_file = dir;
  // forward to end
  while(*remote_file != 0x00) remote_file++;
  // back to last '/'
  while((remote_file != dir) && (*remote_file != '/')) remote_file--;
  if(*remote_file == '/')
  {
    *remote_file = 0x00;
    remote_file++;
  }
  else // remote file in root dir
  {
    dir = root_dir;
  }
  
  HINTERNET hInternet = ::InternetOpen(HTTP_USER_AGENT, INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, 0);
  if(hInternet == NULL)
  {
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

    HINTERNET hConnect = ::InternetConnect(hInternet, host, INTERNET_INVALID_PORT_NUMBER, user, pass, INTERNET_SERVICE_FTP, INTERNET_FLAG_PASSIVE, 0);
  if(hConnect == NULL)
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  if(!FtpSetCurrentDirectory(hConnect, dir))
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  if(!FtpDeleteFile(hConnect, remote_file))
  {
    ::InternetCloseHandle(hConnect);
    ::InternetCloseHandle(hInternet);
    InterlockedExchange((long *)&net::last_err, (long)net::ERR);
    if(par != NULL) delete[]par;
    return 0;
  }

  ::InternetCloseHandle(hConnect);
  ::InternetCloseHandle(hInternet);
  InterlockedExchange((long *)&net::last_err, (long)net::OK);
  if(par != NULL) delete[]par;

  return 1;
}

/* ----------------- END: WinInet functions --------------- */
