#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>

int main() {
  const int N = 365;
  const int M = 30;
  const int R = 1000000;
  
  int n = 0;
  std::srand(std::time(NULL));
  for (int r = 0; r < R; ++r) {
    std::vector<int> days(N, 0);
    bool found = false;
    for (int i = 0; i < M && !found; ++i) {
      int j = std::rand() % N;
      days[j] += 1;
      if (days[j] > 1) found = true;
    }
    if (found) n += 1;  
  }
  std::cout << n * 1.0 / R << std::endl;
  return 0;
}
