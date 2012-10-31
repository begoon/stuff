/*
  XMPP tag class.
  by Alexander S. Pristenski 2007

  Change log:

  16.01.2007  Alexander S. Pristenski - initial creation.
*/

#ifndef _XMPP_TAG_H_
#define _XMPP_TAG_H_

#include <windows.h>
#include <list>
#include "xmpp_att.h"

class XMPP_tag
{
public:

   XMPP_tag(void);
   XMPP_tag(const XMPP_tag &t);
  ~XMPP_tag(void);

  XMPP_tag& operator=(const XMPP_tag &t);

  const char* get_name(void);
  const char* get_body(void);

  void set_name(const char *s);
  void set_name(const char *pbeg, const char *pend);
  void set_body(const char *s);
  void set_body(const char *pbeg, const char *pend);

  const char* get_attribute(const char *name);
  bool        set_attribute(const char *name, const char *value);

  XMPP_tag* get_sub_tag(const char *name);

  void clear(void);

  unsigned long length(void) const;
  char* compose(void);

  char* parse(const char *pbeg, const char *pend);

  bool parse_error; // sets to TRUE on parse error
  bool closed;      // "tag is closed" flag
  std::list<XMPP_attribute> *attributes;
  std::list<XMPP_tag> *sub_tags;

  //thread-safety procedures
  void Lock();
  void Unlock();

private:

  void  preprocess(void);
  void postprocess(void);

  char *name;
  char *body;
  CRITICAL_SECTION cs;
};

#endif