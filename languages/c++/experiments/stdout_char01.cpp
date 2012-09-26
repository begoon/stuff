#include <iostream>
#include <string>

int main() {
  std::string a1 = "some \x01value\x01";
  std::string a2 = "some value";

  std::cout << "a1: [" << a1 << "], " << a1.length() << std::endl;
  std::cout << "a2: [" << a2 << "], " << a2.length() << std::endl;
  return 0;
}

