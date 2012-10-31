/*
  XMPP tag class.
  by Alexander S. Pristenski 2007

  Change log:

  16.01.2007  Alexander S. Pristenski - initial creation.
  30.01.2007  Alexander S. Pristenski - bugfix: parse() tag "/name" composing was incorrect.
*/

#include <windows.h>
#include "utf8.h"
#include "xmpp_tag.h"

struct
{
  char symbol;
  const char *analog;
}
replace[] =
{
  { '&',  "&amp;" },
  { '<',  "&lt;"  },
  { '>',  "&gt;"  },
  { '\'', "&apos;"},
  { '\"', "&quot;"},
  { NULL, NULL  }
};

/*
  Name: XMPP_tag()

  Description: default constructor.

  Parameters: none.

  Return value: none.

  Revision: 16.01.2007
*/
XMPP_tag::XMPP_tag(void)
{
  InitializeCriticalSection(&cs);
  
  parse_error = false;
  closed = false;
  name = NULL;
  attributes = new std::list<XMPP_attribute>();
  body = NULL;
  
  sub_tags = new std::list<XMPP_tag>();
}

/*
  Name: XMPP_tag()

  Description: copy constructor.

  Parameters:
        [in] t - constant link to another XMPP_tag.

  Return value: none.

  Revision: 16.01.2007
*/
XMPP_tag::XMPP_tag(const XMPP_tag &t)
{
  InitializeCriticalSection(&cs);

  parse_error = t.parse_error;
  closed = t.closed;
  
  if(t.name != NULL)
  {
    name = new char[lstrlen(t.name) + 1];
    if(name == NULL) return;
    lstrcpy(name, t.name);
  }
  else
  {
    name = NULL;
  }
  
  attributes = new std::list<XMPP_attribute>();
  if(attributes == NULL) return;
  attributes->assign(t.attributes->begin(), t.attributes->end());
  
  if(t.body != NULL)
  {
    body = new char[lstrlen(t.body) + 1];
    if(body == NULL) return;
    lstrcpy(body, t.body);
  }
  else
  {
    body = NULL;
  }

  sub_tags = new std::list<XMPP_tag>();
  if(sub_tags == NULL) return;
  sub_tags->assign(t.sub_tags->begin(), t.sub_tags->end());
}

/*
  Name: ~XMPP_tag()

  Description: default destructor.

  Parameters: none.

  Return value: none.

  Revision: 16.01.2007
*/
XMPP_tag::~XMPP_tag(void)
{
  if(name != NULL) { delete[](name); name = NULL; }
  if(body != NULL) { delete[](body); body = NULL; }
  if(attributes != NULL) { attributes->clear(); delete attributes; }
  if(attributes != NULL) { sub_tags->clear(); delete sub_tags; }

  DeleteCriticalSection(&cs);  
}

void XMPP_tag::Lock()
{
  EnterCriticalSection(&cs);  
}

void XMPP_tag::Unlock()
{
  LeaveCriticalSection(&cs);  
}

/*
  Name: operator=()

  Description: operator=.

  Parameters:
        [in] t - constant link to another XMPP_tag.

  Return value: none.

  Revision: 16.01.2007
*/
XMPP_tag& XMPP_tag::operator=(const XMPP_tag &t)
{
  clear(); // clear existing values first

  parse_error = t.parse_error;
  closed = t.closed;
  
  if(t.name != NULL)
  {
    name = new char[lstrlen(t.name) + 1];
    if(name == NULL) return *this;
    lstrcpy(name, t.name);
  }
  else
  {
    name = NULL;
  }
  
  if(!(t.attributes->empty()))
  {
    attributes->clear();
    attributes->assign(t.attributes->begin(), t.attributes->end());
  }
  else
  {
    attributes->clear();
  }
  
  if(t.body != NULL)
  {
    body = new char[lstrlen(t.body) + 1];
    if(body == NULL) return *this;
    lstrcpy(body, t.body);
  }
  else
  {
    body = NULL;
  }

  if(!(t.sub_tags->empty()))
  {
    sub_tags->clear();
    sub_tags->assign(t.sub_tags->begin(), t.sub_tags->end());
  }
  else
  {
    sub_tags->clear();
  }

  return *this;
}

/*
  Name: get_name()

  Description: retrieve a name of the tag.

  Parameters: none.

  Return value: constant pointer to name of the tag.

  Revision: 16.01.2007
*/
const char* XMPP_tag::get_name(void)
{
  return name;
}

/*
  Name: get_body()

  Description: retrieve a body of the tag.

  Parameters: none.

  Return value: constant pointer to body of the tag.

  Revision: 16.01.2007
*/
const char* XMPP_tag::get_body(void)
{
  return body;
}

/*
  Name: set_name()

  Description: sets a name of the tag.

  Parameters:
        [in] s - new name of the tag.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::set_name(const char *s)
{
  if(!s) return;
  if(name != NULL) { delete[](name); name = NULL; }
  name = new char[lstrlen(s) + 1];
  if(name == NULL) return;
  lstrcpy(name, s);
}

/*
  Name: set_name()

  Description: sets a name of the tag.

  Parameters:
        [in] pbeg - pointer to the begining of a new name
                    of the tag in a string.
        [in] pend - pointer to the end of a new name
                    of the tag in a string.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::set_name(const char *pbeg, const char *pend)
{
  if((pbeg >= pend) || (!pbeg) || (!pend)) return;
  if(name != NULL) { delete[](name); name = NULL; }
  name = new char[(pend - pbeg + 1)];
  if(name == NULL) return;
  lstrcpyn(name, pbeg, (pend - pbeg + 1));
}

/*
  Name: set_body()

  Description: sets a body of the tag.

  Parameters:
        [in] s - new body of the tag.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::set_body(const char *s)
{
  if(!s) return;
  if(body != NULL) { delete[](body); body = NULL; }
  body = new char[lstrlen(s) + 1];
  if(body == NULL) return;
  lstrcpy(body, s);
}

/*
  Name: set_body()

  Description: sets a body of the tag.

  Parameters:
        [in] pbeg - pointer to the begining of a new body
                    of the tag in a string.
        [in] pend - pointer to the end of a new body
                    of the tag in a string.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::set_body(const char *pbeg, const char *pend)
{
  if((pbeg >= pend) || (!pbeg) || (!pend)) return;
  if(body != NULL) { delete[](body); body = NULL; }
  body = new char[(pend - pbeg + 1)];
  if(body == NULL) return;
  lstrcpyn(body, pbeg, (pend - pbeg + 1));
}

/*
  Name: get_attribute()

  Description: retrieve a value of the attribute with a given name.

  Parameters:
        [in] name - a name of the attribute of current tag.

  Return value:
          ON SUCCESS: const pointer to a value of
                the attribute of current tag.
          ON   ERROR: NULL.

  Revision: 16.01.2007
*/
const char* XMPP_tag::get_attribute(const char *name)
{
  if(!name) return NULL;
  std::list<XMPP_attribute>::iterator i;

  for(i = attributes->begin(); i != attributes->end(); ++i)
  {
    if(!lstrcmp(i->get_name(), name))
    {
      return i->get_value();
    }
  }
  return NULL;
}

/*
  Name: set_attribute()

  Description: sets a value of the attribute with a given name.

  Parameters:
        [in] name - a name of the attribute of current tag.
        [in] value - a value to set.

  Return value:
          ON SUCCESS: true.
                (attribute exists and value is set)
          ON   ERROR: false.

  Revision: 16.01.2007
*/
bool XMPP_tag::set_attribute(const char *name, const char *value)
{
  if((!name) || (!value)) return false;
  std::list<XMPP_attribute>::iterator i;

  for(i = attributes->begin(); i != attributes->end(); ++i)
  {
    if(!lstrcmp(i->get_name(), name))
    {
      i->set_value(value);
      return true;
    }
  }
  return false;
}

/*
  Name: get_sub_tag()

  Description: searches sub tag of current tag by name.

  Parameters:
        [in] name - a name of the sub tag.

  Return value:
          ON SUCCESS: pointer to sub tag (XMPP_tag) found.
          ON   ERROR: NULL.

  IMPORTANT NOTE: the function searches sub tag
          in the SAME LEVEL OF NESTING!

          If you want to search whole XMPP -
          you should call it on sub tags too!

  Revision: 16.01.2007
*/
XMPP_tag* XMPP_tag::get_sub_tag(const char *name)
{
  XMPP_tag *sub_tag = NULL;

  if(sub_tags->empty()) return sub_tag;

  std::list<XMPP_tag>::iterator i;

  for(i = sub_tags->begin(); i != sub_tags->end(); i++)
  {
    if(!lstrcmp(i->get_name(), name))
    {
      sub_tag = &(*i);
      break;
    }
  }

  return sub_tag;
}

/*
  Name: clear()

  Description: clears all tag data.

  Parameters: none.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::clear(void)
{
  parse_error = false;
  closed = false;

  if(name != NULL) { delete[](name); name = NULL; }
  attributes->clear();
  if(body != NULL) { delete[](body); body = NULL; }
  sub_tags->clear();
}

/*
  Name: length()

  Description: calculates tag length in text - called from compose().

  Parameters: none.

  Return value:
          ON SUCCESS: length of tag and sub tags in text.
          ON   ERROR: '0'

  IMPORTANT NOTE: If you intend to change this function,
          you should change compose() accordingly!

  Revision: 16.01.2007
*/
unsigned long XMPP_tag::length(void) const
{
  unsigned long size = 0;

  if(!name) return size;

  // size of open tag
  size++; // '<'
  size += lstrlen(name);

  std::list<XMPP_attribute>::iterator i;

  for(i = attributes->begin(); i != attributes->end(); ++i)
  {
    if(i == attributes->begin()) size++; // ' '

    size += lstrlen(i->get_name());
    size += 2; // "='"
    size += lstrlen(i->get_value());

    if((++i) == attributes->end())
    {
      size += 1; // "'"
      --i;
    }
    else
    {
      size += 2; // "' "
      --i;
    }
  }

  // if tag has only attributes, i.e.:
    // <name XMPP_attribute1='value' XMPP_attribute2='value' ... />
  if((closed) && (!body) && (sub_tags->empty()))
  {
    size += 3; // "/>\0"
    return size;
  }

  size++; // '>'

  // size of body    
  if(body != NULL)
  {
    size += lstrlen(body);
  }

  // size of sub-tags
  if(!(sub_tags->empty()))
  {
    std::list<XMPP_tag>::iterator j;

    for(j = sub_tags->begin(); j != sub_tags->end(); ++j)
    {
      size += j->length();
    }
  }

  // size of close tag
  if(closed)
  {
    size += 2; // "</"
    size += lstrlen(name);
    size += 2; // ">\0"
  }
  return size;
}

/*
  Name: compose()

  Description: composes internal tag structures to text (XML).

  Parameters: none.

  Return value:
          ON SUCCESS: pointer to text (XML).
          ON   ERROR: NULL.

  IMPORTANT NOTE: If you intend to change this function,
          you should change length() accordingly!

          Text (XML) created in newly allocated
          dynamic memory - DON'T FORGET TO FREE IT!

  Revision: 16.01.2007
*/
char* XMPP_tag::compose(void)
{
  // postprocess this tag body and all subtags bodies
  if(!(sub_tags->empty()))
  {
    std::list<XMPP_tag>::iterator j;

    for(j = sub_tags->begin(); j != sub_tags->end(); ++j)
    {
      j->postprocess();
    }
  }

  // get length of this tag and all subtags
  char *xmpp = new char[length()];
  if(xmpp == NULL) return NULL;

  if(!name && (xmpp != NULL)) { delete[](xmpp); return NULL; }

  // compose open tag (or closed tag w/o body and sub-tags)
  wsprintf(xmpp, "<%s", name);

  std::list<XMPP_attribute>::iterator i;

  for(i = attributes->begin(); i != attributes->end(); ++i)
  {
    if(i == attributes->begin()) lstrcat(xmpp, " ");

    lstrcat(xmpp, i->get_name());
    lstrcat(xmpp, "=");
    lstrcat(xmpp, ATTRIBUTE_DELIMITER);
    lstrcat(xmpp, i->get_value());
    
    if((++i) == attributes->end())
    {
      lstrcat(xmpp, ATTRIBUTE_DELIMITER);
      --i;
    }
    else
    {
      lstrcat(xmpp, ATTRIBUTE_DELIMITER);
      lstrcat(xmpp, " ");
      --i;
    }
  }

  // if tag has only attributes, i.e.:
    // <name XMPP_attribute1='value' XMPP_attribute2='value' ... />
  if((closed) && (!body) && (sub_tags->empty()))
  {
    lstrcat(xmpp, "/>");
    return xmpp;
  }

  lstrcat(xmpp, ">");

  // compose body (call to postprocess() placed in length())
  if(body) lstrcat(xmpp, body);

  // compose sub-tags
  if(!(sub_tags->empty()))
  {
    std::list<XMPP_tag>::iterator j;

    for(j = sub_tags->begin(); j != sub_tags->end(); ++j)
    {
      char *tmp = j->compose();
      lstrcat(xmpp, tmp);
      if(tmp != NULL) delete[](tmp);
    }
  }

  // compose close tag
  if(closed)
  {
    lstrcat(xmpp, "</");
    lstrcat(xmpp, name);
    lstrcat(xmpp, ">");
  }

  return xmpp;
}

/*
  Name: parse()

  Description: parses text (XML) and fills internal tag structures.

  Parameters:
        [in] pbeg - pointer to the beginning
              of text (XML) to parse.
        [in] pend - pointer to the end
              of text (XML) to parse.

  Return value:
          ON SUCCESS: pend+1
          ON   ERROR: a) NULL on unrecoverable error
                   (overlapped tags, for example).
                b) pointer to the error in text (XML)
                   (incomplete tag, for example).

  Revision: 30.01.2007
*/
char* XMPP_tag::parse(const char *pbeg, const char *pend)
{
  clear(); // clear this tag before parse

  // check initial conditions
  if((pbeg >= pend) || (!pbeg) || (!pend))
  {
    parse_error = true;   
    return NULL;
  }

  const char *to_beg = NULL;
  const char *to_end = NULL;
  const char *tc_beg = NULL;
  const char *tc_end = NULL;

  // find '<' and '>' for open tag
  to_beg = strchr(pbeg, '<');

  if(0 == to_beg) // tag open begin symbol not found!
  {
    parse_error = true;
    return NULL;
  }

  if(to_beg > pend) // out of right boundary!
  {
    parse_error = true;
    return NULL;
  }

  to_end = strchr(to_beg, '>');

  if(0 == to_end) // tag open end symbol not found!
  {
    parse_error = true;
    return (char *)pend;
  }

  if(to_end > pend) // out of right boundary!
  {
    parse_error = true;
    return NULL;
  }

  // extract name
  const char *name_beg = NULL;
  const char *name_end = NULL;
  
  name_beg = (to_beg + 1);
  while(*name_beg && isspace(*name_beg)) name_beg++;

  // "unecpected tag close" error: '</tag>' w/o '<tag>'
  if(*name_beg == '/')
  {
    parse_error = true;
    return NULL;
  }

  if(name_beg >= to_end) // tag has no name: '<   >'
  {
    parse_error = true;
    return NULL;
  }

  name_end = name_beg;

  while (!isspace(*name_end) && (*name_end != '/') && (name_end < to_end)) name_end++;

  set_name(name_beg, name_end);

  // extract attributes (if exists)
  const char *an_beg = name_end;
  const char *an_end = NULL;
  const char *av_beg = NULL;
  const char *av_end = NULL;

  XMPP_attribute attribute;

  while(an_beg < to_end)
  {
    // find attribute name begin
    while(*an_beg && isspace(*an_beg)) an_beg++;

    // is it closed tag?
    if(*an_beg == '/') break;

    if(an_beg > to_end) break;
    an_end = an_beg;
    while (!isspace(*an_end) && (*an_end != '=') && (an_end < to_end)) an_end++;
    if(an_end > to_end) break;

    attribute.set_name(an_beg, an_end);

    // skip "<spaces>=<spaces>"
    av_beg = an_end;
    while(*av_beg && isspace(*av_beg)) av_beg++;

    if(av_beg > to_end) break;

    // expected '='
    if(*av_beg != '=') break;
    av_beg++;

    while(*av_beg && isspace(*av_beg)) av_beg++;
    if(av_beg > to_end) break;

    // find attribute value
    av_beg = strpbrk(av_beg, ATTRIBUTE_DELIMITERS);
    if(av_beg > to_end) break;
    av_beg++;
    av_end = strpbrk(av_beg, ATTRIBUTE_DELIMITERS);
    if(av_end > to_end) break;

    attribute.set_value(av_beg, av_end);

    attributes->push_back(attribute);

    an_beg = ++av_end;
  }

  // check closed tag "/>"
  if(*(to_end - 1) == '/') // '/' right before '>'
  {
    closed = true;
    return ((char *)(to_end + 1));
  }

  // find close tag </name>
  {
    char *closename = new char[lstrlen(name) + 2];
    if(closename == NULL) return NULL;
    lstrcpy(closename, "/");
    lstrcat(closename, name);

    tc_beg = strstr((to_end + 1), closename);

    if(closename != NULL) delete[]closename;

    if(0 == tc_beg) // tag close '/name' not found
    {
      parse_error = true;
      return (char *)pend;
    }

    // find '<' before "/name" (should be only space delimited!!!)
    tc_beg--; // skip '/' of "/name"
    while((to_end < tc_beg) && isspace(*tc_beg)) tc_beg--;

    if(*tc_beg != '<') // '<' for '</name>' not found
    {
      parse_error = true;
      return NULL;
    }

    tc_end = strchr(tc_beg, '>');

    if(0 == tc_end) // '>' for '</name>' not found
    {
      parse_error = true;
      return (char *)pend; // perhaps not complete tag
    }

    if(tc_end > pend) // out of right boundary!
    {
      parse_error = true;
      return NULL;
    }

    closed = true;
  }

  // extract tag body (if exists)
  const char *body_beg = (to_end + 1);
  const char *body_end = NULL;

  body_end = strchr(body_beg, '<');

  if(body_end > tc_beg)
  {
    return NULL;
  }

  if((body_end - body_beg) > 0)
  {   
    set_body(body_beg, body_end);
    preprocess();
  }

  // parse sub_tags
  const char *st_beg = NULL;
  const char *st_end = (tc_beg - 1);

  if(body)
    st_beg = body_end;
  else
    st_beg = to_end + 1;

  XMPP_tag sub_tag;

  while(st_beg < st_end)
  {
    sub_tag.clear();

    st_beg = sub_tag.parse(st_beg, st_end);

    if(sub_tag.parse_error == false)
    {
      sub_tags->push_back(sub_tag);
    }
    else
    {
      break;
    }
  }

  return (char *)(tc_end + 1);
}

/*
  Name: preprocess()

  Description: decodes tag body from UTF-8 and then
               tags body's quoted symbols to printable symbols
         before show/process received message.

  Parameters: none.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::preprocess(void)
{
  if(body == NULL) return;

  char *new_s = utf8_decode(body);
  if(body != NULL) delete[]body;
  body = new_s;
  new_s = NULL;

  new_s = new char[(lstrlen(body)+1)];
  if(new_s == NULL) return;
  memset(new_s, 0, (lstrlen(body)+1));

  char *i = body;
  char *j = new_s;

  while(*i != 0)
  {
    bool replaced = false;

    if(*i == '&')
    {
      for(unsigned int k = 0; replace[k].analog != NULL; k++)
      {       
        if(!strncmp(i, replace[k].analog, lstrlen(replace[k].analog)))
        {
          *j = replace[k].symbol;
          i += lstrlen(replace[k].analog);
          j++;
          replaced = true;
          break;
        }
      }
    }

    if(replaced != true)
    {
      *j = *i;
      i++;
      j++;
    }
  }

  if(body != NULL) delete[]body;
  body = new_s;
}

/*
  Name: postprocess()

  Description: encodes tags body's printable symbols
         to quoted symbols and then encodes
         tag body to UTF-8 before send the message.

  Parameters: none.

  Return value: none.

  Revision: 16.01.2007
*/
void XMPP_tag::postprocess(void)
{
  if(body == NULL) return;

  // pre-calc new length
  // because replacements are bigger than special symbols
  unsigned long new_body_len = 1; // 1 - for trailing "\0"
  char *i = body;

  while(*i != 0)
  {
    bool replaced = false;

    for(unsigned int k = 0; replace[k].symbol != NULL; k++)
    {
      if(*i == replace[k].symbol) // found replacement!
      {
        // to avoid double postprocess!!!
        if(*i == '&')
        {
          for(unsigned int k = 0; replace[k].analog != NULL; k++)
          {       
            if(!strncmp(i, replace[k].analog, lstrlen(replace[k].analog)))
            {
              return;
            }
          }
        }
        // end: to avoid double postprocess!!!

        new_body_len += lstrlen(replace[k].analog);
        i++;
        replaced = true;
        break;
      }
    }

    if(replaced != true) // regular symbol
    {
      i++;
      new_body_len += 3; // maybe UTF-8
    }
  }

  // perform replacement
  char *new_s = new char[new_body_len];
  if(new_s == NULL) return;
  memset(new_s, 0, new_body_len);

  i = body;
  char *j = new_s;

  while(*i != 0)
  {
    bool replaced = false;

    for(unsigned int k = 0; replace[k].symbol != NULL; k++)
    {
      if(*i == replace[k].symbol) // found replacement!
      {
        lstrcat(new_s, replace[k].analog);
        j += lstrlen(replace[k].analog);
        i++;
        replaced = true;
        break;
      }
    }

    if(replaced != true) // regular symbol
    {
      *j = *i;
      i++;
      j++;
    }
  }

  if(body != NULL) delete[]body;
  body = new_s;
  new_s = NULL;

  new_s = utf8_encode(body);
  if(body != NULL) delete[]body;
  body = new_s;
  new_s = NULL;
}
