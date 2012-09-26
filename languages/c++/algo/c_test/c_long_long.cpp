#include <iostream>

// The last value is C(30, 61)
long long C(int m, int n) {
  long long p = 1;
  for (int i = 1; i <= m; i++) {
    p *= n - i + 1;
    p /= i;
  }
  return p;
}

int main() {
  unsigned long long m = ((unsigned long long)-1) >> 1;
  std::cout << m << std::endl;
  for (int i = 1; ; ++i) {
    long long r = C(i/2, i);
    if (r < 0) {
      std::cout << i - 1 << std::endl;
      break;
    }
  }
  std::cout << C(30, 61) << std::endl;
  return 0;
}
