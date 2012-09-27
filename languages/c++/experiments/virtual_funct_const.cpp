#include <iostream>

class A {
 public:
  A() {
    construct();
  }

  ~A() {
    destruct();
  }

  virtual void construct() {
    std::cout << "A::construct()" << std::endl;
  }

  virtual void destruct() {
    std::cout << "A::destruct()" << std::endl;
  }
};

class B: public A {
 public:
  B() {
    construct();
  }

  ~B() {
    destruct();
  }

  virtual void construct() {
    std::cout << "B::construct()" << std::endl;
  }

  virtual void destruct() {
    std::cout << "B::destruct()" << std::endl;
  }
};

int main() {
  B b;
  return 0;
}
