#include "xportgpio.h"

AnsiString XPortGPIO::hex_dump( const AnsiString& str, const AnsiString& delimiter )
{
   AnsiString result = "";

   for( int i = 1; i <= str.Length(); i++ )
      result += IntToHex( (int)(unsigned char)str[ i ], 2 ) + delimiter;

   return result;
}

AnsiString XPortGPIO::exec_cmd( const AnsiString& cmd )
{
   TIdTCPClient* client = new TIdTCPClient(NULL);

   AnsiString data;

   try {
      client->Host = host;
      client->Port = port;

      try {
         client->Connect();
      } catch( EIdSocketError& e ) {
         throw Exception( AnsiString().sprintf( "Unable to connect to %s:%d (0x%X)", client->Host.c_str(), client->Port, client->Port ) );
      }

      client->WriteBuffer( cmd.data(), cmd.Length(), true );

      std::time_t started = std::time(NULL);
      while( client->CurrentReadBufferSize() < 1 && std::time(NULL) - started < timeout )
         client->ReadFromStack( true, timeout );

      if( client->CurrentReadBufferSize() < 1 )
         throw Exception( "There is no response from XPort device" );

      data = client->ReadString( 1 );
      if( (unsigned char)data[1] == 0xFF )
         throw Exception( "XPort device has returned 0xFF (wrong command)" );

      started = std::time(NULL);
      while( client->CurrentReadBufferSize() < 4 && std::time(NULL) - started < timeout )
         client->ReadFromStack( true, timeout);

      if( client->CurrentReadBufferSize() < 4 )
         throw Exception( "There is no response from XPort device" );

      data += client->ReadString( 4 );

      if( data[1] != cmd[1] )
         throw Exception(
            AnsiString().sprintf(
               "Wrong XPort response, first character must be 0x%02X but 0x%02X received [%s]",
               (int)(unsigned char)cmd[1], (int)(unsigned char)data[1], hex_dump(data).c_str()
            )
         );

   } __finally {
      delete client;
   }

   return data;
}

XPortGPIO::Values XPortGPIO::to_values( const AnsiString& str )
{
   return
      (XPortGPIO::Values)(unsigned char)str[1] <<  0 |
      (XPortGPIO::Values)(unsigned char)str[2] <<  8 |
      (XPortGPIO::Values)(unsigned char)str[3] << 16 |
      (XPortGPIO::Values)(unsigned char)str[4] << 24
   ;
}

AnsiString XPortGPIO::from_values( XPortGPIO::Values first, XPortGPIO::Values second )
{
   AnsiString v;
   v.SetLength( 8 );

   v[1] = first  >>  0 & 0xff;
   v[2] = first  >>  8 & 0xff;
   v[3] = first  >> 16 & 0xff;
   v[4] = first  >> 24 & 0xff;
   v[5] = second >>  0 & 0xff;
   v[6] = second >>  8 & 0xff;
   v[7] = second >> 16 & 0xff;
   v[8] = second >> 24 & 0xff;

   return v;
}

XPortGPIO::Values XPortGPIO::GetFunctions()
{
   return to_values( exec_cmd( AnsiString( "\x10\x00\x00\x00\x00\x00\x00\x00\x00", 9 ) ).SubString( 2, 4 ) );
}

XPortGPIO::Values XPortGPIO::GetDirections()
{
   return to_values( exec_cmd( AnsiString( "\x11\x00\x00\x00\x00\x00\x00\x00\x00", 9 ) ).SubString( 2, 4 ) );
}

XPortGPIO::Values XPortGPIO::GetActiveLevels()
{
   return to_values( exec_cmd( AnsiString( "\x12\x00\x00\x00\x00\x00\x00\x00\x00", 9 ) ).SubString( 2, 4 ) );
}

XPortGPIO::Values XPortGPIO::GetCurrentStates()
{
   return to_values( exec_cmd( AnsiString( "\x13\x00\x00\x00\x00\x00\x00\x00\x00", 9 ) ).SubString( 2, 4 ) );
}

XPortGPIO::Values XPortGPIO::SetDirections( XPortGPIO::Values mask, XPortGPIO::Values directions )
{
   return to_values( exec_cmd( AnsiString( "\x19" ) + from_values(mask, directions) ).SubString( 2, 4 ) );
}

XPortGPIO::Values XPortGPIO::SetActiveLevels( XPortGPIO::Values mask, XPortGPIO::Values levels )
{
   return to_values( exec_cmd( AnsiString( "\x1a" ) + from_values(mask, levels) ).SubString( 2, 4 ) );
}

XPortGPIO::Values XPortGPIO::SetStates( XPortGPIO::Values mask, XPortGPIO::Values states )
{
   return to_values( exec_cmd( AnsiString( "\x1b" ) + from_values(mask, states) ).SubString( 2, 4 ) );
}
