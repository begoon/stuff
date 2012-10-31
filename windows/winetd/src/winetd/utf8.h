/*
	UTF-8 encode/decode.

	Change log:

	16.01.2007	Alexander S. Pristenski - initial creation.
*/

#ifndef _UTF8_H_
#define _UTF8_H_

char* __stdcall utf8_encode(const char* str);
char* __stdcall utf8_decode(const char* str);

#endif
