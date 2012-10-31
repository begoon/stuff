#ifndef _INIPARSE_H
#define _INIPARSE_H

#include <string.h>

struct option_t {
  char name[32];
  char value[64];
};

extern struct option_t options[100];

void trim_trailing_crlf(char* buf);
void parse_string(char* buf, char** name, char** value);
size_t parse(const char* filename);
void print(size_t sz);

#endif
