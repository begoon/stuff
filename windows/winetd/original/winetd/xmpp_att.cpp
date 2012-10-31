/*
	XMPP attribute class.
	by Alexander S. Pristenski 2007

	Change log:

	16.01.2007	Alexander S. Pristenski - initial creation.
*/

#include <windows.h>
#include "xmpp_att.h"

/*
	Name: XMPP_attribute()

	Description: default constructor.

	Parameters: none.

	Return value: none.

	Revision: 16.01.2007
*/
XMPP_attribute::XMPP_attribute(void)
{
	name  = NULL;
	value = NULL;
}

/*
	Name: XMPP_attribute()

	Description: constructor vith parameters.

	Parameters:
				[in] n - name of attribute.
				[in] v - value of attribute.

	Return value: none.

	Revision: 16.01.2007
*/
XMPP_attribute::XMPP_attribute(const char *n, const char *v)
{
	name = new char[lstrlen(n) + 1];
	if(name == NULL) return;
	lstrcpy(name, n);
	value = new char[lstrlen(v) + 1];
	if(value == NULL) return;
	lstrcpy(value, v);
}

/*
	Name: XMPP_attribute()

	Description: copy constructor.

	Parameters:
				[in] a - link to another XMPP_attribute.

	Return value: none.

	Revision: 16.01.2007
*/
XMPP_attribute::XMPP_attribute(const XMPP_attribute &a)
{
	name = new char[lstrlen(a.name) + 1];
	if(name == NULL) return;
	lstrcpy(name, a.name);
	value = new char[lstrlen(a.value) + 1];
	if(value == NULL) return;
	lstrcpy(value, a.value);
}

/*
	Name: ~XMPP_attribute()

	Description: default destructor.

	Parameters: none.

	Return value: none.

	Revision: 16.01.2007
*/
XMPP_attribute::~XMPP_attribute(void)
{
	if(name  != NULL) { delete[](name);  name = NULL;  }
	if(value != NULL) { delete[](value); value = NULL; }
}

/*
	Name: get_name()

	Description: retrieve a name of the attribute.

	Parameters: none.

	Return value: constant pointer to name of the attribute.

	Revision: 16.01.2007
*/
const char* XMPP_attribute::get_name(void)
{
	return name;
}

/*
	Name: get_value()

	Description: retrieve a value of the attribute.

	Parameters: none.

	Return value: constant pointer to value of the attribute.

	Revision: 16.01.2007
*/
const char* XMPP_attribute::get_value(void)
{
	return value;
}

/*
	Name: set_name()

	Description: sets a name of the attribute.

	Parameters:
				[in] s - new name of the attribute.

	Return value: none.

	Revision: 16.01.2007
*/
void XMPP_attribute::set_name(const char *s)
{
	if(!s) return;
	if(name != NULL) { delete[](name); name = NULL; }
	name = new char[lstrlen(s) + 1];
	if(name == NULL) return;
	lstrcpy(name, s);
}

/*
	Name: set_name()

	Description: sets a name of the attribute.

	Parameters:
				[in] pbeg - pointer to the begining of a new name
				            of the attribute in a string.
				[in] pend - pointer to the end of a new name
				            of the attribute in a string.

	Return value: none.

	Revision: 16.01.2007
*/
void XMPP_attribute::set_name(const char *pbeg, const char *pend)
{
	if((pbeg >= pend) || (!pbeg) || (!pend)) return;
	if(name != NULL) { delete[](name); name = NULL; }
	name = new char[(pend - pbeg + 1)];
	if(name == NULL) return;
	lstrcpyn(name, pbeg, (pend - pbeg + 1));
}

/*
	Name: set_value()

	Description: sets a value of the attribute.

	Parameters:
				[in] s - new value of the attribute.

	Return value: none.

	Revision: 16.01.2007
*/
void XMPP_attribute::set_value(const char *s)
{
	if(!s) return;
	if(value != NULL) { delete[](value); value = NULL; }
	value = new char[lstrlen(s) + 1];
	if(value == NULL) return;
	lstrcpy(value, s);
}

/*
	Name: set_value()

	Description: sets a value of the attribute.

	Parameters:
				[in] pbeg - pointer to the begining of a new value
				            of the attribute in a string.
				[in] pend - pointer to the end of a new value
				            of the attribute in a string.

	Return value: none.

	Revision: 16.01.2007
*/
void XMPP_attribute::set_value(const char *pbeg, const char *pend)
{
	if((pbeg >= pend) || (!pbeg) || (!pend)) return;
	if(value != NULL) { delete[](value); value = NULL; }
	value = new char[(pend - pbeg + 1)];
	if(value == NULL) return;
	lstrcpyn(value, pbeg, (pend - pbeg + 1));
}
