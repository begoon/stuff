#include <iostream>

template<int I>
struct A{
  char a[I];
};

A<sizeof(A<10>)> a;

int main() {
  std::cout << sizeof(a) << std::endl;
}
