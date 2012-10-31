#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct option_t {
  char name[32];
  char value[64];
} options[100];

size_t parse(const char* filename) {
  FILE* f;
  size_t sz = 0;

  f = fopen(filename, "r");
  if (!f) return 0;

  while (!feof(f)) {
    char buf[256];
    char* p;
    fgets(buf, sizeof(buf), f);
    p = strchr(buf, '=');
    *p = 0;
    strcpy(options[sz].name, buf);
    strcpy(options[sz].value, p + 1);
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
