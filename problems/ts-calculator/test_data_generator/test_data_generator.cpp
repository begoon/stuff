#include <iostream>
#include <fstream>
#include <cassert>

int main(int argc, const char* argv[]) {
  assert(argc > 1);
  std::ofstream os(argv[1], std::ios::out | std::ios::binary);
  double g;
  while (std::cin >> g) {
    os.write(reinterpret_cast<char*>(&g), sizeof(g));
  }
  return 0;
}