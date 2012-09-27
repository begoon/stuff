#include <iostream>
#include <vector>
#include <cstdlib>
#include <cmath>

int main(int argc, char* argv[]) {
  int n = argc > 1 ? std::atoi(argv[1]) : 100;

  std::cout << n << std::endl;

  int sqrt_n = static_cast<int>(std::sqrt(static_cast<double>(n))) + 1;

  std::vector<TYPE> S(n, true);

  for (int i = 2; i < sqrt_n; ++i)
    if (S[i])
      for (int j = i*i; j < n; j+=i)
        S[j] = false;

  int count = 0;
  for (int i = 2; i < n; ++i)
    if (S[i])
      count++;

  std::cout << count << std::endl;

  return 0;
}

