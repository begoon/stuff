//---------------------------------------------------------------------------

#pragma hdrstop
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#include <utilcls.h> // TComInterface
//---------------------------------------------------------------------------

#pragma argsused

#include <system.hpp>

#include <string>
#include <cstdio>

std::string getSystemMessage( int error )
{
   static char msg[1024];

   FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM |
      FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL,
      error,
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
      msg,
      sizeof msg,
      NULL
   );

   return msg;
}

void error( const char* msg, int error )
{
   std::fprintf( stderr, "Error: %s, 0x%08X, %s\n", msg, error, getSystemMessage(error).c_str() );
   exit(0);
}

int main( int argc, char* argv[] )
{
   if( argc < 2 ) {
      std::fprintf( stderr, "usage: %s lnkfile\n", argv[0] );
      exit(0);
   }

   TComInterface<IShellLink> SL;

   TInitOleT<int> init;

   HRESULT hr = SL.CreateInstance( CLSID_ShellLink );
   if( FAILED(hr) ) error( "CreateInstance()", hr );

   TComInterface<IPersistFile> PF=SL; //конструктор, инкапсулирующий QueryInterface, можно также использовать operator=

   hr = PF->Load(WideString(argv[1]).c_bstr(),0);
   if( FAILED(hr) )
      error( "Load()", hr );

   char args[1024];
   hr = SL->GetArguments( args, sizeof args );
   if( FAILED(hr) )
      error( "GetArguments()", hr );

   char path[1024];
   hr = SL->GetPath( path, sizeof path, NULL, 0 );
   if( FAILED(hr) )
      error( "GetPath()", hr );

   std::printf( "%s %s", path, args );

   return 0;
}
//---------------------------------------------------------------------------
