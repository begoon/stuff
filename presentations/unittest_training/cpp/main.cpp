#include <iostream>

#include "iniparse.h"

int main(int argc, char* argv[]) {
  if (argc < 2) return 1;

  IniParser parser(argv[1]);

  std::cout << parser << std::endl;

  return 0;
}
