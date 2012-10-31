/*
	XMPP attribute class.
	by Alexander S. Pristenski 2007

	Change log:

	16.01.2007	Alexander S. Pristenski - initial creation.
*/

#ifndef _XMPP_ATT_H_
#define _XMPP_ATT_H_

#define ATTRIBUTE_DELIMITERS "\"'"
#define ATTRIBUTE_DELIMITER  "'"

class XMPP_attribute
{
public:

	 XMPP_attribute(void);
	 XMPP_attribute(const char *n, const char *v);
	 XMPP_attribute(const XMPP_attribute &a);
	~XMPP_attribute(void);

	const char* get_name(void);
	const char* get_value(void);
	void set_name(const char *s);
	void set_name(const char *pbeg, const char *pend);
	void set_value(const char *s);
	void set_value(const char *pbeg, const char *pend);

private:

	char *name;
	char *value;
};

#endif
