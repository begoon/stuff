#include <iostream>

// The last value is C(29, 15)
int C(int m, int n) {
  int p = 1;
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
    int r = C(i/2, i);
    if (r < 0) {
      std::cout << i - 1 << std::endl;
      break;
    }
  }
  std::cout << C(14, 29) << std::endl;
  std::cout << C(15, 29) << std::endl;
  return 0;
}
