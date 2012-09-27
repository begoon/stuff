#include <iostream>

class X {
 public:
  virtual void f(int i = 0) {
    std::cout << "X::f(): " << i << std::endl;
  }
};

class Y: public X {
 public:
  void f(int i = 1) {
    std::cout << "Y::f(): " << i << std::endl;
  }
};

int main() {
  X* a = new Y;
  a->f();

  Y* b = new Y;
  b->f();

  return 0;
}
