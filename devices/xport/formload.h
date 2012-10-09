#ifndef _FORMLOAD_H
#define _FORMLOAD_H

#include <vcl.h>

class FormLoader {
public:
   static void FormLoader::store( TForm* form, AnsiString file = "" );
   static void FormLoader::restore( TForm* form, AnsiString file = "" );
};

#endif
