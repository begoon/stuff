#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "iniparse.h"

void trim_trailing_crlf(char* s) {
  size_t sz = strlen(s);
  char* p;

  if (sz == 0) return;
  p = s + sz - 1;

  while (sz-- && (*p == '\n' || *p == '\r'))
    *p-- = 0;
}

void parse_string(char* buf, char** name, char** value) {
  *name = buf;
  *value = strchr(buf, '=');
  **value = 0;
  *value += 1;

  trim_trailing_crlf(*value);
}

#ifdef UNITTESTING
#define feof(f) _mock_feof(f)
#define fgets(buf, sz, f) _mock_fgets(buf, sz, f)
#endif

struct option_t options[100];

size_t parse(const char* filename) {
  FILE* f;
  size_t sz = 0;

  f = fopen(filename, "r");
  if (!f) return -1;

  while (!feof(f)) {
    char buf[256];
    char* name;
    char* value;
    fgets(buf, sizeof(buf), f);
    trim_trailing_crlf(buf);
    if (strlen(buf) == 0) continue;
    parse_string(buf, &name, &value);
    strcpy(options[sz].name, name);
    strcpy(options[sz].value, value);
    sz += 1;
  }

  fclose(f);

  return sz;
}

void print(size_t sz) {
  size_t i;
  for (i = 0; i < sz; ++i) {
    printf("%s = [%s]\n", options[i].name, options[i].value);
  }
}
