/*
	UTF-8 encode/decode.

	Change log:

	16.01.2007	Alexander S. Pristenski - initial creation.
*/

#include <windows.h>
#include "utf8.h"

/*
	Name: utf8_decode()

	Description: decodes string from UTF-8.

	Parameters:
				[in] const char* str - pointer to UTF-8 string
									   (type conversion to char *)

	Return value:
					ON SUCCESS - string in ANSI codepage.
					ON   ERROR - NULL.

	Revision: 16.01.2007
*/
char* __stdcall utf8_decode(const char* str)
{
	if(str == NULL) return NULL;

	int len = lstrlen(str);

	if(len < 2)
	{
		char *res = new char[2];

		if(res == NULL) return NULL;

		*res = *str;
		*(res+1) = '\0';
		return res;
	}

	wchar_t* tempBuf = new wchar_t[(( len+1 )*sizeof( wchar_t ))];

	if(tempBuf == NULL) return NULL;

	memset(tempBuf, 0, (( len+1 )*sizeof( wchar_t )));

	{
		wchar_t* d = tempBuf;
		BYTE* s = ( BYTE* )str;

		while( *s )
		{
			if (( *s & 0x80 ) == 0 )
			{
				*d++ = *s++;
				continue;
			}

			if (( s[0] & 0xE0 ) == 0xE0 && ( s[1] & 0xC0 ) == 0x80 && ( s[2] & 0xC0 ) == 0x80 )
			{
				*d++ = (( WORD )( s[0] & 0x0F ) << 12 ) + ( WORD )(( s[1] & 0x3F ) << 6 ) + ( WORD )( s[2] & 0x3F );
				s += 3;
				continue;
			}

			if (( s[0] & 0xE0 ) == 0xC0 && ( s[1] & 0xC0 ) == 0x80 )
			{
				*d++ = ( WORD )(( s[0] & 0x1F ) << 6 ) + ( WORD )( s[1] & 0x3F );
				s += 2;
				continue;
			}

			*d++ = *s++;
		}

		*d = 0;
	}

	char *res = new char[len+1];

	if(res == NULL)
	{
		if(tempBuf != NULL) delete[]tempBuf;
		return NULL;
	}

	memset(res, 0, len+1);

	WideCharToMultiByte(CP_ACP, 0, tempBuf, -1, res, len, NULL, NULL);

	if(tempBuf != NULL) delete[]tempBuf;

	return res;
}

/*
	Name: utf8_encode()

	Description: encodes string to UTF-8.

	Parameters:
				[in] const char* str - pointer to a string
									   in ANSI codepage.

	Return value:
					ON SUCCESS - string in UTF-8
								 (type conversion to char *)
					ON   ERROR - NULL.

	Revision: 16.01.2007
*/
char* __stdcall utf8_encode(const char* str)
{
	if(str == NULL) return NULL;

	// Convert local codepage to unicode
	int len = lstrlen(str);
	WCHAR* wszTemp = new WCHAR[(sizeof(WCHAR)*(len+1))];

	if(wszTemp == NULL) return NULL;

	MultiByteToWideChar(CP_ACP, 0, str, -1, wszTemp, len+1 );

	/* convert unicode to utf8 */
	const WCHAR* w;
	len = 0;

	for ( w = wszTemp; *w; w++ )
	{
		if ( *w < 0x0080 ) len++;
		else if ( *w < 0x0800 ) len += 2;
		else len += 3;
	}

	unsigned char* szOut = new unsigned char[(len+1)];

	if(szOut == NULL)
	{
		if(wszTemp != NULL) delete[]wszTemp;
		return NULL;
	}

	int i = 0;
	for ( w = wszTemp; *w; w++ )
	{
		if(*w < 0x0080)
		{
			szOut[i++] = ( unsigned char ) *w;
		}
		else if ( *w < 0x0800 )
		{
			szOut[i++] = 0xc0 | (( *w ) >> 6 );
			szOut[i++] = 0x80 | (( *w ) & 0x3f );
		}
		else
		{
			szOut[i++] = 0xe0 | (( *w ) >> 12 );
			szOut[i++] = 0x80 | (( ( *w ) >> 6 ) & 0x3f );
			szOut[i++] = 0x80 | (( *w ) & 0x3f );
		}
	}

	szOut[ i ] = '\0';
	/* end: convert unicode to utf8 */

	if(wszTemp != NULL) delete[]wszTemp;

	return ( char* )szOut;
}
