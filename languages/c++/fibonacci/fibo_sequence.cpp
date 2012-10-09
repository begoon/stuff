#include <iostream>
#include <cstdlib>

void iterative(int N) {
  int a = 1;
  int b = 1;
  std::cout << a << " " << b << " ";
  for (int i = 0; i < N; i++) {
    int c = a + b;
    std::cout << c << " ";
    a = b;
    b = c;
  }
  std::cout << std::endl;
}

// f(x) = f(x - 2) + f(x - 1)

void do_recursive(int N, int i, int a, int b) {
  std::cout << b << " ";
  if (i < N)
    do_recursive(N, i + 1, b, a + b);
}

void recursive(int N) {
  int a = 1, b = 1;
  std::cout << a << " ";
  do_recursive(N, 0, a, b);
}

int main(int argc, char* argv[]) {
  int N = argc > 1 ? std::atoi(argv[1]) : 10;
  std::cout << "N = " << N << std::endl;

  std::cout << "Iterative method" << std::endl;
  iterative(N);

  std::cout << "Recursive method" << std::endl;
  recursive(N);
  return 0;
}
