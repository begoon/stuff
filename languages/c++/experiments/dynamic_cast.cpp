#include <iostream>

class A { virtual void f() {} };
class B {};

int main() {
  A a;
  try {
    B& b = dynamic_cast<B&>(a);
    if (&b == 0) {
      // ...
    }
  } catch (...) {
    std::cout << "Got it!" << std::endl;
  }
  return 0;
}