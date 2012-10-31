#include <stdlib.h>

size_t parse(const char* filename);
void print(size_t sz);

int main(int argc, char* argv[]) {
  size_t sz;

  if (argc < 2) exit(1);

  sz = parse(argv[1]);

  return 0;
}

