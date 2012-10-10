#ifndef _XPORTGPIO_H
#define _XPORTGPIO_H

#include <vcl.h>
#include <IdTCPClient.hpp>

#include <ctime>

class XPortGPIO {
public:
   typedef unsigned long Values; 
private:
   AnsiString host;
   int port;
   int timeout;

   AnsiString hex_dump( const AnsiString& str, const AnsiString& delimiter = "" );

   AnsiString exec_cmd( const AnsiString& cmd );

   Values to_values( const AnsiString& str );
   AnsiString from_values( Values first, Values second );

public:
   XPortGPIO( const AnsiString& a_host, int a_port = 0x77F0, int a_timeout = 5 ) :
      host( a_host ), port( a_port ), timeout( a_timeout )
   {}

   void set_host( const AnsiString& a_host ) { host = a_host; }
   void set_port( int a_port ) { port = a_port; }

   Values GetFunctions();
   Values GetDirections();
   Values GetActiveLevels();
   Values GetCurrentStates();
   Values SetDirections( Values mask, Values directions );
   Values SetActiveLevels( Values mask, Values levels );
   Values SetStates( Values mask, Values states );
};

#endif
